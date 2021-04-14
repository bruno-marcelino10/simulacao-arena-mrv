##### Simulação de Monte Carlo: Projeto Arena MRV ##########################
##### 31/03/2021 ###########################################################
##### Bruno Marcelino ######################################################
##### Dados base retirados de doc. do Atlético fornecido aos conselheiros ##

# Nota: Alguns dados (como a depreciação) foram estimados a partir das demonstrações financeiras de estádios semelhantes como o Allianz Parque.

### --- Importando Bibliotecas --- ###

library("tvm")
library("triangle")

### --- Funções utilizadas --- ###

# VPL: npv(k, fluxos de caixa)
# TIR: irr(fluxos de caixa)

# Quantidade de simulações 
simulacoes <- 1000

# Quantidade de Fluxos de Caixa que serão dados de forma aleatória
n <- 21

# Inflação esperada
g <- 0.0275

ajuste_inf <- function(fluxo, tempo){
    tempo <- seq(1:tempo)
    fluxo <- fluxo*(1+g)^tempo
    return(fluxo)
}

# Calculando o custo de oportunidade do shopping
taxa <- function(pv, fv, n){
    i = ((fv/pv)^(1/n)) - 1
    return(i)    
    }

k <- taxa(250, 290, 2)

# Vetores nulos

    # Avaliação  
    vpl <- vector('numeric', length = simulacoes)
    tir <- vector('numeric', length = simulacoes)
    
    # Fluxos de Caixa
    fluxos_de_caixa <- matrix(nrow = simulacoes, ncol = n+3) # n fluxos de caixa aleatórios + n investimentos fixos (0, 1 e 2)
    
    # Variáveis
    outras_rec <- vector('numeric', length = simulacoes)
    rec_bilheteria <- vector('numeric', length = simulacoes)
    dep <- vector('numeric', length = simulacoes)
    desp_operacionais <- vector('numeric', length = simulacoes)
    
for (i in seq(1:simulacoes)){
    
    ##### Inputs
    
    ### Receitas Fixas
    
    # T1
    receita_cadeiras_t1 <- 100000000 * 0.8
    receita_camarotes_t1 <- 80000000 * 0.92
    receita_naming_rights_t1 <- 60000000
    
    # T2
    receita_cadeiras_t2 <- 100000000 * 0.2
    receita_camarotes_t2 <- 80000000 * 0.08
    
    # Fluxos de Receitas Fixas
    fluxos_fixos <- c(receita_cadeiras_t1 + receita_camarotes_t1 + receita_naming_rights_t1,
                      receita_cadeiras_t2 + receita_camarotes_t2)
    
    ### Receitas Variáveis
    
    # Receitas com Novos Negócios
    outras_receitas <- rtriangle(n, 17592700, 17592700 + 40000000)
    outras_rec[i] <- mean(outras_receitas)
    
    # Receitas com Bilheteria
    copa_brasil <- as.integer(runif(n, 1, 10))
    libertadores <- as.integer(runif(n, 3, 7))
    jogos <- copa_brasil + libertadores + 27
    receita_bilheteria <- jogos * 43.2 * 24083 # Jogos * Ingresso * Público
    rec_bilheteria[i] <- mean(receita_bilheteria)
    
    ### Despesas
    
    # Despesas Operacionais (excl. dep)
    receita_variavel <- outras_receitas + receita_bilheteria
    despesas_operacionais <- (19195000/mean(receita_variavel)) * receita_variavel
    desp_operacionais[i] <- despesas_operacionais
    
    # Depreciação
    arrecadacao_inicial <- 510000000
    terreno <- 50000000
    depreciacao <- runif(n, 0.0166, 0.0333) * (arrecadacao_inicial) # Balanço do Palmeiras
    dep[i] <- mean(depreciacao)
    
    # Lucro Operacional Líquido
    lucro_bruto_operacional <- receita_bilheteria + outras_receitas - despesas_operacionais - depreciacao
    fluxo_de_caixa_livre <- lucro_bruto_operacional + depreciacao
    
    # Ajuste do FCl pelo g 
    fluxo_de_caixa_livre <- ajuste_inf(fluxo_de_caixa_livre, n-3)
    
    # Investimento Inicial
    investimento <- arrecadacao_inicial + terreno
    
    # Custo Médio Ponderado do Capital = k (variável já declarada)
    
    # Perpetuidade Constante (a partir de t = 24): pode ser acrescentada conforme o código abaixo (deixarei comentado)
    # perpetuidade <- fluxo_de_caixa_livre[n] / (k - g)
    # fluxo_de_caixa_livre[n] <- fluxo_de_caixa_livre[n] + perpetuidade
    
    # Valor Contábil Líquido (a ser acrescentado ao final do projeto para critérios de realização do imóvel)
    valor_residual <- arrecadacao_inicial - sum(depreciacao) 
    fluxo_de_caixa_livre[n] <- fluxo_de_caixa_livre[n] + valor_residual
    
    ##### Parâmetros
    fluxos <- c(-investimento, fluxos_fixos, fluxo_de_caixa_livre)
    fluxos_de_caixa[i,] <- fluxos
    
    ##### Critérios de Avaliação
    vpl[i] <- npv(k, fluxos)/1000000
    tir[i] <- irr(fluxos)
    }

# Dados das variáveis testadas
variaveis <- data.frame("Outras Receitas" = outras_rec,
                    "Bilheteria" = rec_bilheteria, 
                    "Depreciação" = dep,
                    "Despesas Operacionais" = desp_operacionais,
                    "VPL" = vpl,
                    "TIR" = tir
                    )

head(variaveis)

# Gráficos dos Resultados
par(mfrow = c(1,2))

hist(vpl,
     main = "VPL",
     col = 2,
     xlab = "Valor Presente Líquido",
     ylab = "Frequência",
     cex.axis=1)

hist(tir,
     main = "TIR",
     col = 4,
     xlab = "Taxa Interna de Retorno",
     ylab = "",
     cex.axis=1)

# Input das Estatísticas do VPL e TIR
media <- c(mean(vpl), mean(tir))
dp <- c(sd(vpl), sd(tir))
maximo <- c(max(vpl), max(tir))
minimo <- c(min(vpl), min(tir))
mediana <- c(median(vpl), median(tir))

prop_positivo <- c( # Proporção na qual IRR e VPL se encontram positivos
    sum(ifelse(vpl >= 0, 1, 0))/simulacoes,
    sum(ifelse(tir >= 0, 1, 0))/simulacoes
) 

quantil_inf <- c(quantile(vpl, 0.025), quantile(tir, 0.025))
quantil_inf <- unname(quantil_inf) # Limite Inferior IC 5%

quantil_sup <- c(quantile(vpl, 0.975), quantile(tir, 0.975))
quantil_sup <- unname(quantil_sup) # Limite Superior IC 5%

# Tabela com as estatísticas
estatisticas <- rbind(media, dp, maximo, minimo, mediana, prop_positivo, quantil_inf, quantil_sup)
colnames(estatisticas) <- c("VPL (em milhões)", "TIR (termos percentuais)") 
rownames(estatisticas) <- c("Média", "Desvio-Padrão", "Máximo", "Mínimo", "Mediana", "Proporção > 0", "Quantil 2.5%", "Quantil 97,5%")
estatisticas[ ,2] <- estatisticas[ ,2] * 100
estatisticas[6,2] <- estatisticas[6,2]/100
estatisticas <- round(estatisticas, 2)

print(estatisticas)






