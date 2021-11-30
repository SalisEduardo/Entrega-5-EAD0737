# ------------------------------------------------
# FEAUSP
# EAD737 - Topicos Avancados de Financas
# Prof. Leandro Maciel (leandromaciel@usp.br)
# Segundo Semestre de 2020
# ------------------------------------------------

# ------------------------------------------------------------------------------
# Ideia: fazer o balanceamento da carteira recomendada (10SIM) pelo BTG do mês de novembro
# O BTG recomenda um balanceamento igual

# Acoes foram obtidas pela API do alphavantage - extracao feita pelo python
# obs: os dados disponiveis sao desde 2021-08-05 ate 021-11-19 , ja que a Raizen 	teve seu IPO recentemente

# ------------------------------------------------------------------------------




#Pacotes

# library(quantmod)   # nao foi necessaria
library(readxl)
library(xts)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(tidyverse)

# ------------------------------------------------------------------------------

df_inicial <- read.csv("port_acoes.csv",header = TRUE)
Acoes <- df_inicial[, -1]
row.names(Acoes) <- df_inicial[,1]


retornos <- Return.calculate(Acoes,method = "log") %>% 
            na.omit()

# ------------------------------------------------

# Avaliar as correlacoes dos retornos:

noAcoes = ncol(retornos) 

matrizCorrelacao = round(cor(as.matrix(retornos)),3)

# ------------------------------------------------

# 80% dos dados dentro da amostra

split_dados <- round(0.8 * nrow(Acoes))

amostra = Acoes[1:split_dados,] # divisao com base nos dados
fora_amostra = Acoes[split_dados:nrow(Acoes),]

summary(amostra) # estatisticas descritivas retornos
