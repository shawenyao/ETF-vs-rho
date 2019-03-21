suppressWarnings(library(tidyverse))

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho")
source("functions/functions_efficient_frontier.R", echo=TRUE)


#==== general setup ====
rf <- 0.01
miu <- c(0.05, 0.15, 0.20)
sigma <- c(0.25, 0.3, 0.35)
rho12 <- 0.1
rho23 <- 0.5
rho13 <- 0.2
rho <- matrix(
  c(1, rho12, rho13, rho12, 1, rho23, rho13, rho23, 1), 
  nrow = length(miu), 
  ncol = length(miu), 
  byrow = TRUE
)
atomic_assets <- tibble(volatility = sigma, return = miu)


#==== global minimum variance portfolio ====
weight_minimum <- calculate_minimum_variance_portfolio(miu, sigma, rho)
minimum_portfolio <- calculate_return_volatility(list(weight_minimum), miu, sigma, rho)


#==== tangency portfolio ====
weight_tangency <- calculate_tangency_portfolio(rf, miu, sigma, rho)
tangency_portfolio <- calculate_return_volatility(list(weight_tangency), miu, sigma, rho)


#==== efficient frontier ====
efficient_frontier_weights <- calculate_efficient_frontier(miu, sigma, rho, min_return = 0.01, max_return = 0.22, step = 0.005)
efficient_frontier <- calculate_return_volatility(efficient_frontier_weights, miu, sigma, rho)


#==== plot ====
ggplot(efficient_frontier, aes(x = volatility, y = return)) +
  # the efficient frontier
  geom_path(size = 2, linejoin = "round", lineend = "round", color = "gray66") +
  # the atomic assets
  geom_point(data = atomic_assets, color = "dodgerblue3", size = 5) +
  geom_point(data = atomic_assets, color = "white", size = 2) +
  # the global minimum variance portfolio
  geom_point(data = minimum_portfolio, color = "chartreuse4", size = 5) +
  geom_point(data = minimum_portfolio, color = "white", size = 2) +
  # the tangency portfolio
  geom_point(data = tangency_portfolio, color = "coral3", size = 5) +
  geom_point(data = tangency_portfolio, color = "white", size = 2) +
  theme_minimal()
