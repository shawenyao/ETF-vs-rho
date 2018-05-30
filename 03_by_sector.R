#== sector-wise SP500 constituents return correlations
sp500_sector_daily_return <- sp500_daily_price %>% 
  select(date, symbol, weight, sector, adjusted, volume) %>% 
  # delete duplicate rows
  distinct() %>% 
  # delete missing rows
  na.omit() %>% 
  # delete 0 volume prices
  filter(volume >0) %>% 
  group_by(date, sector) %>% 
  summarise(
    adjusted = sum(weight * adjusted)
  ) %>% 
  group_by(sector) %>% 
  # daily return
  mutate(
    return = adjusted / lag(adjusted) - 1
  ) %>% 
  select(-adjusted) %>% 
  # delete the the missing value in the first row due to using lag()
  na.omit() %>% 
  spread(sector, return) %>% 
  mutate(
    month = date %>% as.yearmon() %>% as.Date(frac = 1)
  ) %>% 
  select(month, date, everything())


sp500_sector_monthly_correlation_matrix <- sp500_sector_daily_return %>%
  split(.$month) %>%
  lapply(
    function(sp500_daily_return_for_a_month){
      print(sp500_daily_return_for_a_month %>% nrow())
      
      sp500_daily_return_for_a_month %>% 
        select(-month, -date) %>% 
        cor()
    }
  )
