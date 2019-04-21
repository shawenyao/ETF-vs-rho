suppressWarnings(library(tidyverse))
suppressWarnings(library(beepr))

set.seed(350)

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho")
source("functions/functions_efficient_frontier.R", echo = FALSE)
source("functions/functions_save_svg.R", echo = TRUE)


#==== general setup ====
risky_assets <- tibble(return = c(0.05, 0.15, 0.16), volatility = c(0.20, 0.24, 0.28))
risk_free_asset <- tibble(return = 0.01, volatility = 0)

# correlation matrix
rho12 <- 0.1
rho23 <- 0.5
rho13 <- 0.2
rho <- matrix(
  c(1, rho12, rho13, rho12, 1, rho23, rho13, rho23, 1), 
  nrow = nrow(risky_assets), 
  ncol = nrow(risky_assets), 
  byrow = TRUE
)


#==== plot =====
plot1 <- plot_efficient_frontier(
  risky_assets = risky_assets, 
  rho = rho,
  risk_free_asset = risk_free_asset
)


#==== output ====
save_svg(plot = plot1, file_name = "output/efficient_frontier1.svg", width = 5, height = 3)

# play sound when finished
beep(sound = 2)
