source("functions/functions_plot_correlation_matrix.R")
source("functions/functions_save_jpg.R")
source("functions/functions_save_png.R")
source("functions/functions_save_svg.R")

#==== sector-wise SP500 constituents return correlations ====
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

sector_corr_by_month <- lapply(
  sp500_sector_monthly_correlation_matrix,
  function(corr){
    corr %>% 
      as.data.frame() %>% 
      mutate(sector = colnames(.)) %>% 
      gather(sector2, correlation, -sector)
  }
) %>% 
  bind_rows(.id="month") %>% 
  mutate(
    month = month %>% as.Date()
  )

#==== plot ====
for(i in seq_along(sp500_sector_monthly_correlation_matrix)){
  
  month <- sp500_sector_daily_return$month %>% 
    unique() %>% 
    as.yearmon() %>% 
    nth(i)
  
  print(month)
  
  corr_plot <- plot_correlation_matirx(
    correlation_matrix = sp500_sector_monthly_correlation_matrix[[i]],
    title = month %>% as.character()
  )
  
  save_jpg(
    corr_plot, print_plot = FALSE, width = 600, height = 600,
    file_name = paste0("Output/sector_", format(month, "%Y%m"), ".jpg")
  )
}
