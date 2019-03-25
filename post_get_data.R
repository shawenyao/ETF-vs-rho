suppressWarnings(library(tidyverse))
suppressWarnings(library(magrittr))
suppressWarnings(library(rvest))
suppressWarnings(library(rio))
suppressWarnings(library(tidyquant))
suppressWarnings(library(beepr))

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho/")
source("functions/functions_calculate_daily_returns.R")


#==== the daily retun of sp500 ====
sp500_daily_return <- tq_get(
  "^GSPC",
  from = "1957-03-04",
  to = "2019-03-24"
) %>% 
  mutate(symbol = "sp500") %>% 
  calculate_daily_returns()

export(sp500_daily_return, file = "input/sp500_daily_return.csv")


#==== the list of sp500 constituents and their first dates of entry ====
sp500_constituents <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies" %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="constituents"]') %>% 
  html_table() %>% 
  extract2(1)

export(sp500_constituents, file = "input/sp500_constituents.csv")


#==== the daily return of sp500 constituents around their inclusion dates ====
half_window <- 1
sp500_new_addition <- sp500_constituents %>% 
  mutate(t0 = as.Date(`Date first added`, format = "%Y-%m-%d")) %>% 
  filter(!is.na(t0)) %>% 
  mutate(
    t_start = t0 %>% as.yearmon() %>% subtract(half_window) %>% as.Date(frac = 0),
    t_end = t0 %>% as.yearmon() %>% add(half_window) %>% as.Date(frac = 1)
  )

sp500_new_addition_return <- list(
  x = sp500_new_addition$Symbol,
  from = sp500_new_addition$t_start,
  to = sp500_new_addition$t_end
) %>% 
  pmap(function(x, from, to){

    # query by tidyquant
    query <- tq_get(x = x, from = from, to = to)
    
    if(!is.data.frame(query)){
      # if no such symbol can be found during the period specified
      return(NULL)
    }else{
      # if query returns successfully, append the symbol
      query %>% mutate(symbol = x)
    }
  }) %>% 
  bind_rows() %>% 
  calculate_daily_returns()
  
export(sp500_new_addition_return, file = "input/sp500_new_addition_return.csv")

warnings()

# play sound when finished
beep(sound = 2)
