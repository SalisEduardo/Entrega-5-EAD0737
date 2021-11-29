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

# ------------------------------------------------------------------------------

Acoes <- read.csv("port_acoes.csv",header = TRUE)

names(Acoes)[1] <- "Data" # renomeando primeira coluna como data

retornos <- Acoes

# ------------------------------------------------

# Avaliar as correlacoes dos retornos:

noAcoes = ncol(Acoes) - 1 # numero de acoes (primeira coluna datas)

matrizCorrelacao = round(cor(as.matrix(Acoes[,2:(noAcoes+1)])),3)*100

# ------------------------------------------------

# 80% dos dados dentro da amostra

split_dados <- round(0.8 * nrow(Acoes))

amostra = Acoes[1:split_dados,] # divisao com base nos dados
fora_amostra = Acoes[split_dados:nrow(Acoes),]

summary(amostra) # estatisticas descritivas retornos
