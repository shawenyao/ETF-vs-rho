library(tidyquant)

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho")

load("./Input/data.RData")

#=== pairwise SP500 constituents return correlations
sp500_daily_return <- sp500_daily_price %>% 
  select(date, symbol, adjusted) %>% 
  # delete duplicate rows
  distinct() %>% 
  group_by(symbol) %>% 
  # daily return
  mutate(return = adjusted / lag(adjusted) - 1) %>% 
  select(-adjusted) %>% 
  na.omit() %>% 
  spread(symbol, return) %>% 
  mutate(
    month = date %>% as.yearmon() %>% as.Date(frac=1)
  ) %>% 
  select(month, date, everything())

sp500_monthly_correlation_matrix <- lapply(
  split(sp500_daily_return, sp500_daily_return$month),
  function(sp500_daily_return_for_a_month){
    print(sp500_daily_return_for_a_month %>% nrow())
    
    sp500_daily_return_for_a_month %>% 
      select(-month, -date) %>% 
      cor()
  }
)

sp500_average_monthly_correlation <- sp500_monthly_correlation_matrix %>% 
  lapply(
    function(correlation_matrix){
      data.frame(
        mean_correlation = mean(correlation_matrix[upper.tri(correlation_matrix)], na.rm = TRUE)
      )
    }
  ) %>% 
  set_names(sp500_daily_return$month %>% unique()) %>% 
  bind_rows(.id = "month") %>% 
  mutate(month = month %>% as.Date())


#== ETF volume $
sp500_etf_monthly_dollar_volume <- sp500_etf_daily_price %>% 
  mutate(
    volume_dollar = volume * adjusted,
    month = date %>% as.yearmon() %>% as.Date(frac=1)
  ) %>% 
  group_by(month) %>% 
  summarise(volume_dollar = sum(volume_dollar))


#== regression
df <- sp500_average_monthly_correlation %>% 
  left_join(sp500_etf_monthly_dollar_volume, by = "month")

lm(mean_correlation~volume_dollar, df) %>% summary()

df %>% 
  ggplot(aes(x = volume_dollar, y = mean_correlation)) +
  geom_point()+
  geom_smooth(method = 'lm', formula = y~x)

cor(df$mean_correlation,df$volume_dollar)
