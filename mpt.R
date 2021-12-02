# ------------------------------------------------
# FEAUSP
# EAD737 - Topicos Avancados de Financas
# Prof. Leandro Maciel (leandromaciel@usp.br)
# Segundo Semestre de 2020
# ------------------------------------------------

# ------------------------------------------------------------------------------
# Ideia: fazer o balanceamento da carteira recomendada (10SIM) pelo BTG do mês de novembro
# fonte https://www.poupardinheiro.com.br/10-acoes-para-investir-segundo-o-btg-pactual

# Acoes foram obtidas pela API do alphavantage - extracao feita pelo python
# obs: os dados disponiveis sao desde 2019-01-02 ate 2021-12-01 
# dados faltantes; Raizen antes do IPO e IGTA recentemente (troquei pelas acoes "JPSA3.SA")
  # dropei raizen por problemas

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
library(quantmod)   

# ------------------------------------------------------------------------------


#Seleção do período de análise  
startDate = as.Date("2019-01-01")   
endDate = as.Date("2021-11-30")  

#Captura dos dados  

getSymbols("PRIO3.SA" , src = "yahoo", from = startDate, to = endDate)  
#getSymbols("RAIZ4.SA" , src = "yahoo", from = startDate, to = endDate)  
getSymbols("ITUB4.SA" , src = "yahoo", from = startDate, to = endDate)  
getSymbols("JPSA3.SA" , src = "yahoo", from = startDate, to = endDate)
getSymbols("ARZZ3.SA" , src = "yahoo", from = startDate, to = endDate)
getSymbols("ENGI11.SA", src = "yahoo", from = startDate, to = endDate)
getSymbols("SLCE3.SA" , src = "yahoo", from = startDate, to = endDate)
getSymbols("WEGE3.SA" , src = "yahoo", from = startDate, to = endDate)
getSymbols("GGBR4.SA" , src = "yahoo", from = startDate, to = endDate)
getSymbols("PSSA3.SA" , src = "yahoo", from = startDate, to = endDate)

#Cálculo dos retornos  

PRIO3_RET  <- dailyReturn(PRIO3.SA)  
#RAIZ4_RET  <- dailyReturn(RAIZ4.SA)  
ITUB4_RET  <- dailyReturn(ITUB4.SA) 
JPSA3_RET  <- dailyReturn(JPSA3.SA) 
ARZZ3_RET  <- dailyReturn(ARZZ3.SA) 
ENGI11_RET <- dailyReturn(ENGI11.SA) 
SLCE3_RET <- dailyReturn(SLCE3.SA) 
WEGE3_RET <- dailyReturn(WEGE3.SA) 
GGBR4_RET <- dailyReturn(GGBR4.SA) 
PSSA3_RET <- dailyReturn(PSSA3.SA) 

#retornos <- as.data.frame(merge(PRIO3_RET, RAIZ4_RET, ITUB4_RET, JPSA3_RET, ARZZ3_RET, ENGI11_RET, SLCE3_RET, WEGE3_RET, GGBR4_RET, PSSA3_RET))
retornos <- as.data.frame(merge(PRIO3_RET, ITUB4_RET, JPSA3_RET, ARZZ3_RET, ENGI11_RET, SLCE3_RET, WEGE3_RET, GGBR4_RET, PSSA3_RET))


#tickers <- c("PRIO3.SA", "RAIZ4.SA", "ITUB4.SA", "JPSA3.SA","ARZZ3.SA", "ENGI11.SA" , "SLCE3.SA", "WEGE3.SA","GGBR4.SA","PSSA3.SA")

tickers <- c("PRIO3.SA", "ITUB4.SA", "JPSA3.SA","ARZZ3.SA", "ENGI11.SA" , "SLCE3.SA", "WEGE3.SA","GGBR4.SA","PSSA3.SA")



colnames(retornos) <- tickers

retornos
# ------------------------------------------------

# Avaliar as correlacoes dos retornos:

noAcoes = ncol(retornos) 

matrizCorrelacao = round(cor(as.matrix(retornos),use = "pairwise.complete.obs"),3)

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


## split_dados <- round(0.8 * nrow(retornos))
##  amostra = retornos[1:split_dados,] # divisao com base nos dados
##  fora_amostra = retornos[split_dados:nrow(retornos),]

# Periodo de teste de sesempenho da carteira e o mes de novembro

amostra <- retornos[which(row.names(retornos) == "2019-01-02") : which(row.names(retornos) == "2019-10-29"), , drop = FALSE]
fora_amostra <- retornos[which(row.names(retornos) == "2021-11-01") : which(row.names(retornos) == "2021-11-29"), , drop = FALSE]

summary(amostra) # estatisticas descritivas retornos


# Especificacoes da carteira:

fund.names = colnames(retornos) # nome dos ativos

carteira = portfolio.spec(assets = fund.names) # criando a carteira

# Restricao 1 - carteira totalmente investida:

carteira = add.constraint(portfolio = carteira, type = "full_investment")

# Restricao 2 - apenas posicoes compradas:

carteira = add.constraint(portfolio = carteira, type = "long_only")

# Restricao 3 - para os pesos:

carteira = add.constraint(portfolio = carteira, type = "box", min = 0.01, max = 0.2)


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

retorno_esperado_amostra <- mean(Return.portfolio(retornos,weights = extractWeights(MinhaCarteira)))*100
retorno_esperado_amostra

# Verifique a carteira na fronteira eficiente!!!

100*round(MinhaCarteira[["weights"]],4) # pesos em cada ativo (%)


# Alocacoes:

plot(MinhaCarteira)

# Vamos verificar o desempenho dela fora da amostra 

# Calcular os retornos nesse periodo:

RetornoMC = Return.portfolio(fora_amostra,weights = extractWeights(MinhaCarteira))

# Retorno medio (%) fora da amostra:

retorno_medio_teste_MC <- mean(RetornoMC)*100

# Desvio-padrao (%) fora da amostra:

sd_retorno_test_MC  <- sd(RetornoMC)*100

# Sharpe (simplificando a analisa para rf = 0)
sharpe_teste_MC <- retorno_medio_teste_MC/sd_retorno_test_MC

# Vizualizacao:

plot(RetornoMC)

# Visualizar retornos acumulados (soma geometrica dos retornos dia a dia): 

chart.CumReturns(RetornoMC)

# --------------------------------------------------------------------------------------------------------
# comparacao com benchmark de pesos iguais

Retorno_Carteira_EW = Return.portfolio(fora_amostra,weights = carteira$assets)

# Retorno medio EW (%) fora da amostra:

retorno_medio_teste_EW <- mean(Retorno_Carteira_EW)*100

# Desvio-padrao EW (%) fora da amostra:

sd_retorno_test_EW  <- sd(Retorno_Carteira_EW)*100

# Sharpe (simplificando a analisa para rf = 0)
sharpe_teste_EW <- retorno_medio_teste_EW/sd_retorno_test_EW

# Vizualizacao:

plot(Retorno_Carteira_EW)

# Visualizar retornos acumulados (soma geometrica dos retornos dia a dia): 

chart.CumReturns(Retorno_Carteira_EW)









