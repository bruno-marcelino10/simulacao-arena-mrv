##### Cenário Base - Análise de Investimentos Arena MRV ####################
##### 31/03/2021 ###########################################################
##### Bruno Marcelino ######################################################
##### Dados retirados do documento do Atlético fornecido aos conselheiros ##

# Nota: Alguns dados (como a depreciação) foram estimados a partir das demonstrações financeiras de estádios semelhantes como o Allianz Parque.

### --- Importando Bibliotecas --- ###

library("tvm")
library("triangle")

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

# Quantidade de Fluxos de Caixa que serão dados de forma aleatória
n <- 21


##### ----- Cenário Base Inputs ----- #####
##
#

# Custos Iniciais
arrecadacao_inicial <- 510000000
terreno <- 50000000

### Receitas Fixas

# T1
receita_cadeiras_t1 <- 100000000 * 0.8
receita_camarotes_t1 <- 80000000 * 0.92
receita_naming_rights_t1 <- 60000000

# T2
receita_cadeiras_t2 <- 100000000 * 0.2
receita_camarotes_t2 <- 80000000 * 0.08

### Receitas Variáveis 

outras_receitas_base <- (17592700 + 40000000 + 17592700) / 2

receita_bilheteria_base <- 38 * 43.2 * 24083

# Despesas Operacionais (excl. dep)
despesas_operacionais_base <- 19195000

# Depreciação
depreciacao_base <- (0.0166 + 0.0333)/2

##### ----- Cálculo de VPL e TIR para cada input ----- #####
##
#

# Fluxos de Receitas Fixas
fluxos_fixos <- c(receita_cadeiras_t1 + receita_camarotes_t1 + receita_naming_rights_t1,
                  receita_cadeiras_t2 + receita_camarotes_t2)

### Receitas Variáveis 

# Receitas com Novos Negócios
outras_receitas <- rep(outras_receitas_base, n)

# Receitas com Bilheteria
receita_bilheteria <- rep(receita_bilheteria_base, n)  # Jogos * Ingresso * Público

# Despesas Operacionais (excl. dep)
despesas_operacionais <- despesas_operacionais_base

# Depreciação
depreciacao <- rep(depreciacao_base * (arrecadacao_inicial), n) # Balanço do Palmeiras

# Lucro Operacional Líquido
lucro_bruto_operacional <- receita_bilheteria + outras_receitas - despesas_operacionais - depreciacao
fluxo_de_caixa_livre <- lucro_bruto_operacional + depreciacao

# Ajuste do FCl pelo g 
ajuste_inf(fluxo_de_caixa_livre, n)

# Investimento Inicial
investimento <- arrecadacao_inicial + terreno

# Valor Contábil Líquido (a ser acrescentado ao final do projeto para critérios de realização do imóvel)
valor_residual <- arrecadacao_inicial - sum(depreciacao) 
fluxo_de_caixa_livre[n] <- fluxo_de_caixa_livre[n] + valor_residual

##### Parâmetros
FC <- c(-investimento, fluxos_fixos, fluxo_de_caixa_livre)

##### Critérios de Avaliação
vpl <- npv(k, FC)
tir <- irr(FC)

# Tabela com os inputs
cenario_base <- c("Outras Receitas",
                  "Bilheteria",
                  "Desp. Operacionais",
                  "Depreciação",
                  "VPL",
                  "TIR")

cenario_base_2 <- c(round(mean(outras_receitas),2), 
                    round(mean(receita_bilheteria),2),
                    round(mean(despesas_operacionais),2),
                    round(mean(depreciacao),2),
                    round(mean(vpl),2),
                    round(mean(tir),2)
                    )

cenario_base <- data.frame("Variáveis" = cenario_base, "Dados" = cenario_base_2)
print(cenario_base)

