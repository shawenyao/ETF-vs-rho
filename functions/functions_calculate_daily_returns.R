#' calculate returns from prices
#' 
#' @param df a data.frame of prices
#' 
#' @return a data.frame with an additional column: return
#' 
calculate_returns <- function(df){
  
  df %>% 
  # delete duplicate rows
    distinct() %>% 
    # delete missing rows
    na.omit() %>% 
    # delete 0 volume prices
    filter(volume > 0) %>%
    # loop over each symbol
    group_by(symbol) %>% 
    # calculate return
    mutate(
      return = adjusted / lag(adjusted) - 1
    ) %>% 
    ungroup() %>% 
    # delete the the missing value in the first row due to using lag()
    na.omit()
}
