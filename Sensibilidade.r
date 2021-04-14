##### Análise de Sensibilidade a Cenários: Projeto Arena MRV ###############
##### 31/03/2021 ###########################################################
##### Bruno Marcelino ######################################################
##### Dados base retirados de doc. do Atlético fornecido aos conselheiros ##

# Nota: Alguns dados (como a depreciação) foram estimados a partir das demonstrações financeiras de estádios semelhantes como o Allianz Parque.

### --- Importando Bibliotecas --- ###

library("tvm")
library("triangle")
library("tidyverse")

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

##### ----- Teste de Sensibilidade ----- #####
## 
#

simulações <- 1000

vpl <- vector('numeric', length = simulações)
tir <- vector('numeric', length = simulações)

sensibilidade <- function(x){
    x <- seq(from = 0.7*x,
             to = 1.3*x,
             by = (1.3*x - 0.7*x) / simulações
    )
    return(x)
}

for (i in 1:simulações){
    
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
    depreciacao_base <- sensibilidade(depreciacao_base)
    
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
    depreciacao <- rep(depreciacao_base[i] * (arrecadacao_inicial), n) # Balanço do Palmeiras
    
    # Lucro Operacional Líquido
    lucro_bruto_operacional <- receita_bilheteria + outras_receitas - despesas_operacionais - depreciacao
    fluxo_de_caixa_livre <- lucro_bruto_operacional + depreciacao
    
    # Ajuste do FCl pelo g 
    fluxo_de_caixa_livre <- ajuste_inf(fluxo_de_caixa_livre, n)
    
    # Investimento Inicial
    investimento <- arrecadacao_inicial + terreno
    
    # Valor Contábil Líquido (a ser acrescentado ao final do projeto para critérios de realização do imóvel)
    valor_residual <- arrecadacao_inicial - sum(depreciacao) 
    fluxo_de_caixa_livre[n] <- fluxo_de_caixa_livre[n] + valor_residual
    
    ##### Parâmetros
    FC <- c(-investimento, fluxos_fixos, fluxo_de_caixa_livre)
    
    ##### Critérios de Avaliação
    vpl[i] <- npv(k, FC)
    tir[i] <- irr(FC)
    
}

vpl_4 <- vpl

##### Tabela com os inputs
##
#

cenario_base <- c("Outras Receitas",
                  "Bilheteria",
                  "Desp. Operacionais",
                  "Depreciação",
                  "VPL",
                  "TIR")


vpl_sensibilidade <- tibble(vpl_1, vpl_2, vpl_3, vpl_4, "n" = seq(1:simulações))

grafico_sensibilidade <- vpl_sensibilidade %>% 
    ggplot(aes(x = n, y = vpl_1)) +
    geom_line(size = 1.3) +
    theme_bw() + 
    
    geom_line(aes(x = n, y = vpl_2), linetype = "dashed", color = "red", size = 1.3) +
    geom_line(aes(x = n, y = vpl_3), linetype = "dotted", color = "blue", size = 1.3) +
    geom_line(aes(x = n, y = vpl_4), linetype = "twodash", color = "green", size = 1.3) +
    
    geom_vline(xintercept = 500) +
    scale_y_continuous(labels = scales::dollar) +
    
    labs(title = "Análise de Sensibilidade",
         subtitle = "Projeto Arena MRV",
         y = "VPL", x = "") + 
    
    geom_text(x = 900, y = 420000000, label = "Outras Receitas")  +
    geom_text(x = 750, y = 450000000, label = "Bilheteria") +
    geom_text(x = 900, y = 360000000, label = "Desp. Operacionais") + 
    geom_text(x = 925, y = 280000000, label = "Depreciação") 

grafico_sensibilidade

ggsave("figures/Sensibilidade.png") 




