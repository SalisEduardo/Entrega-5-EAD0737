# ------------------------------------------------
# FEAUSP
# EAD737 - Topicos Avancados de Financas
# Prof. Leandro Maciel (leandromaciel@usp.br)
# Segundo Semestre de 2021
# ------------------------------------------------------------------------------
# Autores: Douglas S Cardoso, Eduardo Salis, Nicolas Zanoni
# Ideia: fazer o balanceamento da carteira recomendada (10SIM) pelo BTG do mês de novembro
# Fonte: https://www.poupardinheiro.com.br/10-acoes-para-investir-segundo-o-btg-pactual

# Dependências ------------------------------------------------------------

# library(magrittr)
# library(PortfolioAnalytics)
# library(PerformanceAnalytics)
# library(readxl)
# library(xts)
# library(ROI.plugin.quadprog)
# library(ROI.plugin.glpk)
# library(tidyverse)
# library(corrplot)
# library(quantmod)   
# library(corrr)

# Importando pacotes ------------------------------------------------------
library(magrittr)
library(PortfolioAnalytics)
library(PerformanceAnalytics)

# Captura dos dados -------------------------------------------------------
startDate = as.Date("2019-01-01")   
endDate = as.Date("2021-11-30")  

tickers <- c("PRIO3.SA", "ITUB4.SA", "JPSA3.SA", "ARZZ3.SA", "ENGI11.SA", 
             "SLCE3.SA", "WEGE3.SA", "GGBR4.SA", "PSSA3.SA")

quantmod::getSymbols(tickers , src = "yahoo", from = startDate, to = endDate)  

# Cálculo dos retornos  ---------------------------------------------------
extract_name <- function(df){
  
  df %>% 
    names() %>% 
    purrr::pluck(1) %>% 
    stringr::str_extract(".+(?=.SA.Open)")
}

daily_return <- function(tk){
  
  quantmod::dailyReturn(tk) %>% 
    broom::tidy() %>% 
    dplyr::mutate(ticker = extract_name(tk)) %>% 
    dplyr::select(-series)
}

data_list <- list(PRIO3.SA, ITUB4.SA, JPSA3.SA, ARZZ3.SA, ENGI11.SA, 
                  SLCE3.SA, WEGE3.SA, GGBR4.SA, PSSA3.SA)

tab_ret <- purrr::map_dfr(data_list, daily_return)

returns <- 
  tab_ret %>% 
  tidyr::pivot_wider(
    names_from = ticker,
    values_from = value
  ) %>% 
  dplyr::rename(date = index)

returns

# Avaliar as correlacoes dos retornos -------------------------------------
m_corrr <- returns %>% 
  dplyr::select(-date) %>% 
  corrr::correlate() 

m_corrr %>% 
  corrr::rearrange() %>% 
  corrr::shave() %>% 
  corrr::rplot(colours = "darkred")

# mudando o formato do dado
tidy_corrr <- m_corrr %>% 
  tidyr::pivot_longer(
    cols = PRIO3:PSSA3,
    names_to = "tk_2",
    values_to = "corr"
  ) %>% 
  dplyr::rename(tk_1 = term)
  
# incerteza sobre ser uma boa metrica
tidy_corrr %>% 
  dplyr::summarise(corr_media = mean(corr, na.rm = TRUE))

# pares de acoes com alta correlacao
tidy_corrr %>% 
  dplyr::filter(abs(corr) >= 0.5 & abs(corr) <= 1)

# Montando a carteira -----------------------------------------------------

# divisão da amostra

amostra <- returns %>% dplyr::filter(date <= "2021-10-29")
fora_amostra <- returns %>% dplyr::anti_join(amostra, by = "date")

# estatisticas descritivas retornos
summary(amostra) 

# especificacoes da carteira:
carteira_begin <- portfolio.spec(assets = tickers) 

# Restricao 1 - carteira totalmente investida:
carteira_full_investment <- add.constraint(portfolio = carteira_begin, 
                                          type = "full_investment")

# Restricao 2 - apenas posicoes compradas:
carteira_long_only <- add.constraint(portfolio = carteira_full_investment, 
                          type = "long_only")

# Restricao 3 - para os pesos:
carteira_box = add.constraint(portfolio = carteira_long_only, 
                              type = "box", 
                              min = 0.01, max = 0.2)

# criando um xts dos retornos
xts_amostra <- amostra %>% timetk::tk_xts()

# primeiro jeito
ef_mean_var <- create.EfficientFrontier(R = xts_amostra,
                                        portfolio = carteira_box,
                                        type = "mean-var",
                                        n.portfolios = 100,
                                        )

chart.EfficientFrontier(ef_mean_var,
                        match.col = "StdDev")

# segundo jeito
FE <- meanvar.efficient.frontier(portfolio = carteira_box, 
                                 xts_amostra, 
                                 n.portfolios = 100)
plot(100*FE[,2],
     100*FE[,1],
     xlab="Risco (%)",
     ylab = "Retorno (%)",
     col="blue",
     main = "Fronteira Eficiente")


# Processo de otimizacao --------------------------------------------------

# 1. Carteira de variancia minima (CVM) - eficiente e com menor risco...
carteira_cvm <- add.objective(portfolio = carteira_box, type = "risk", name = "StdDev")

# 2. Carteira de retorno pre definido - eficiente e com menor risco para o retorno desejado...
# carteira <- add.constraint(portfolio = carteira, type = "return",return_target = 0.0008)

my_carteira <- optimize.portfolio(R = xts_amostra,
                                  portfolio = carteira_cvm,
                                  optimize_method = "ROI",
                                  trace = TRUE)

my_carteira 

# Retorno medio (%) da carteira:
Return.portfolio(xts_amostra, weights = extractWeights(my_carteira)) %>% 
  mean() * 100

# A carteira está fronteira eficiente?
round(my_carteira[["weights"]], 4) * 100 # pesos em cada ativo (%)

# Alocacoes:
plot(my_carteira)

# Teste fora da amostra ---------------------------------------------------

# Retornos nesse periodo:
xts_fora_mostra <- fora_amostra %>% timetk::tk_xts()

test_returns = Return.portfolio(xts_fora_mostra,
                             weights = extractWeights(my_carteira))


test_returns %>% 
  broom::tidy() %>% 
  dplyr::select(-series) %>% 
  dplyr::rename(date = index) %>% 
  dplyr::summarise(
    test_mean_return = mean(value) * 100,
    test_sd_return = sd(value) * 100,
    test_sharp = test_mean_return / test_sd_return
  )

# Vizualizacao:

plot(test_returns, col = "darkred", 
     main = "Retornos fora da amostra")

# Visualizar retornos acumulados (soma geometrica dos retornos dia a dia): 

chart.CumReturns(test_returns)

# Comparação --------------------------------------------------------------

ew_returns <- Return.portfolio(xts_fora_mostra, 
                               weights = carteira_cvm$assets) 
  
ew_returns %>% 
  broom::tidy() %>% 
  dplyr::select(-series) %>% 
  dplyr::rename(date = index) %>% 
  dplyr::summarise(
    ew_mean_return = mean(value) * 100,
    ew_sd_return = sd(value) * 100,
    ew_sharp = ew_mean_return / ew_sd_return
  )

# Vizualizacao:
plot(ew_returns)

# Visualizar retornos acumulados (soma geometrica dos retornos dia a dia): 
chart.CumReturns(ew_returns)