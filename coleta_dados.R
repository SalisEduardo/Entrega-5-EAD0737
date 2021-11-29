# ------------------------------------------------
# FEAUSP
# EAD737 - Topicos Avancados de Financas
# Prof. Leandro Maciel (leandromaciel@usp.br)
# Segundo Semestre de 2020
# ------------------------------------------------

# ------------------------------------------------------------------------------
# Ideia: fazer o balanceamento da carteira recomendada (10SIM) pelo BTG do mês de novembro
# O BTG recomenda um balanceamento igual
# ------------------------------------------------------------------------------

#Pacotes


library(quantmod)  
library(PortfolioAnalytics)
library(PerformanceAnalytics)

# ------------------------------------------------
#Seleção do período de análise  
startDate = as.Date("2020-06-01")   
endDate = as.Date("2021-11-22")  


# ------------------------------------------------
#Seleção das ações  
tickers_10SIM_BTG = c('PRIO3.SA','RAIZ4.SA', 'ITUB4.SA','IGTA3.SA', 
            'ARZZ3.SA','ENGL11.SA','SLCE3.SA','WEGE3.SA','GGBR4.SA','PSSA3.SA')  
# ------------------------------------------------
#Captura dos dados  

#retorna um warning pq nem todos os dias temos pregão
getSymbols(tickers_10SIM_BTG, src = "yahoo", from = startDate, to = endDate)  

# ------------------------------------------------
#Cálculo dos retornos  

#PETR_RET <- dailyReturn(PETR4.SA)  


