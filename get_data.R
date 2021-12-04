library(magrittr)

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

readr::write_csv(returns, "dados/returns.csv")
