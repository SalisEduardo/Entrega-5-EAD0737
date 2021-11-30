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
library(corrplot)

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

corrplot(matrizCorrelacao) # plot matriz de correlação

palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = matrizCorrelacao, col = palette, symm = TRUE)

correlacao_media <- mean(matrizCorrelacao) # incerteza sobre ser uma boa metrica

n_pares_acoes_alta_corr <-sum(abs(as.data.frame(matrizCorrelacao))>= 0.5 & abs(as.data.frame(matrizCorrelacao)) < 1)

corr_df <- as.data.frame(matrizCorrelacao)

# selecionando altas correlacoes
corr_df[abs(corr_df) < 0.6 | abs(corr_df) == 1] <- "" 

corr_df

## outra forma

index <- which(abs(matrizCorrelacao) > 0.6 & abs(matrizCorrelacao) < 1,
               arr.ind = T) 
cbind.data.frame(stock1 = rownames(matrizCorrelacao)[index[,1]], 
                 stock2 = colnames(matrizCorrelacao)[index[,2]]) 


# ------------------------------------------------

# 80% dos dados dentro da amostra

split_dados <- round(0.8 * nrow(retornos))

amostra = retornos[1:split_dados,] # divisao com base nos dados
fora_amostra = retornos[split_dados:nrow(retornos),]

summary(amostra) # estatisticas descritivas retornos


# Especificacoes da carteira:

fund.names = colnames(retornos) # nome dos ativos

carteira = portfolio.spec(assets = fund.names) # criando a carteira

# Restricao 1 - carteira totalmente investida:

carteira = add.constraint(portfolio = carteira, type = "full_investment")

# Restricao 2 - apenas posicoes compradas:

carteira = add.constraint(portfolio = carteira, type = "long_only")

# Restricao 3 - para os pesos:

carteira = add.constraint(portfolio = carteira, type = "box", min = 0.05, max = 0.2)


FE = meanvar.efficient.frontier(portfolio = carteira, retornos, n.portfolios = 100)
plot(100*FE[,2],100*FE[,1],xlab="Risco (%)",ylab = "Retorno (%)",col="blue",main = "Fronteira Eficiente")

# ----------------------------------------------------------------------------------------------------------------

# Processo de otimizacao... 

# Definindo o objetivo do investidor:

# 1. Carteira de variancia minima (CVM) - eficiente e com menor risco...

carteira = add.objective(portfolio = carteira, type = "risk", name = "StdDev")

# 2. Carteira de retorno pre definido - eficiente e com menor risco para o retorno desejado...

# carteira <- add.constraint(portfolio = carteira, type = "return",return_target = 0.0008)


# Otimizando a carteira...

MinhaCarteira = optimize.portfolio(R = retornos,portfolio = carteira,optimize_method = "ROI",trace = TRUE)

MinhaCarteira # informacoes da carteira

# Calcular o retorno medio (%) da carteira:

mean(Return.portfolio(retornos,weights = extractWeights(MinhaCarteira)))*100

# Verifique a carteira na fronteira eficiente!!!

100*round(MinhaCarteira[["weights"]],4) # pesos em cada ativo (%)

# Alocacoes:

plot(MinhaCarteira)

# Vamos verificar o desempenho dela fora da amostra 2017 a 2019...

# Calcular os retornos nesse periodo:

RetornoMC = Return.portfolio(fora_amostra,weights = extractWeights(MinhaCarteira))

# Retorno medio (%) fora da amostra:

mean(RetornoMC)*100

# Desvio-padrao (%) fora da amostra:

sd(RetornoMC)*100

# Vizualizacao:

plot(RetornoMC)

# Visualizar retornos acumulados (soma geometrica dos retornos dia a dia): 

chart.CumReturns(RetornoMC)






