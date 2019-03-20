library(tidyverse)

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho")
source("functions/functions_efficient_frontier.R", echo=TRUE)

rf <- 0.01
miu <- c(0.1, 0.15, 0.2)
sigma <- c(0.1, 0.2, 0.3)
rho12 <- 0.5
rho23 <- 0.7
rho13 <- 0.2
rho <- matrix(
  c(1, rho12, rho13, rho12, 1, rho23, rho13, rho23, 1), 
  nrow = length(miu), 
  ncol = length(miu), 
  byrow = TRUE
)

weight_tangent <- calcualte_tangent_portfolio(rf, miu, sigma, rho)
print(weight_tangent)
calculate_return(weight_tangent, miu)
calculate_volatility(weight_tangent, sigma, rho)

