library(tidyquant)
library(beepr)

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho")

###+++ get data +++###
##=== stocks
sp500_list <- tq_index("SP500")

sp500_daily_price <- sp500_list %>% 
  tq_get(
    get = "stock.prices",
    from = "2007-12-31",
    to = "2018-10-31"
  )

#=== ETFs
sp500_etf_list <- c("VOO","IVV","SPY")

sp500_etf_daily_price <- sp500_etf_list %>% 
  tq_get(
    get = "stock.prices",
    from = "2008-01-01",
    to = "2018-10-30"
  )

save(sp500_list, sp500_daily_price, sp500_etf_list, sp500_etf_daily_price, file = "./Input/data.RData")

# play sound when finished
beep(sound = 2)
