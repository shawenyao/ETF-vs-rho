suppressWarnings(library(tidyverse))
suppressWarnings(library(beepr))

set.seed(350)

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho")
source("functions/functions_efficient_frontier.R", echo = FALSE)
source("functions/functions_save_svg.R", echo = TRUE)


#==== general setup ====
risky_assets <- tibble(return = c(0.05, 0.15, 0.16), volatility = c(0.20, 0.24, 0.28))
risky_assets_equilibrium <- tibble(return = c(0.10, 0.127, 0.145), volatility = c(0.20, 0.26, 0.30))
risk_free_asset <- tibble(return = 0.01, volatility = 0)

# correlation matrix
rho12 <- 0.1
rho23 <- 0.5
rho13 <- 0.2
rho_perfect <- 0.9999999
rho <- matrix(
  c(1, rho12, rho13, rho12, 1, rho23, rho13, rho23, 1), 
  nrow = nrow(risky_assets), 
  ncol = nrow(risky_assets), 
  byrow = TRUE
)
rho_equilibrium <- matrix(
  c(1, rho_perfect, rho_perfect, rho_perfect, 1, rho_perfect, rho_perfect, rho_perfect, 1), 
  nrow = nrow(risky_assets), 
  ncol = nrow(risky_assets), 
  byrow = TRUE
)

#==== plot =====
plot1 <- plot_efficient_frontier(
  risky_assets = risky_assets, 
  rho = rho,
  risk_free_asset = risk_free_asset,
  x_range = c(0, 0.44),
  y_range = c(0, 0.25)
)

plot2 <- plot_efficient_frontier(
  risky_assets = risky_assets, 
  rho = rho_equilibrium,
  risk_free_asset = risk_free_asset,
  x_range = c(0, 0.44),
  y_range = c(0, 0.25),
  show_tangency_line = FALSE,
  show_tangency_portfolio = FALSE
)

plot3 <- plot_efficient_frontier(
  risky_assets = risky_assets_equilibrium, 
  rho = rho_equilibrium,
  risk_free_asset = risk_free_asset,
  x_range = c(0, 0.44),
  y_range = c(0, 0.25),
  show_tangency_line = FALSE,
  show_tangency_portfolio = TRUE
)


#==== output ====
save_svg(plot = plot1, file_name = "output/efficient_frontier1.svg", width = 5, height = 3)

save_svg(plot = plot2, file_name = "output/efficient_frontier2.svg", width = 5, height = 3)

save_svg(plot = plot3, file_name = "output/efficient_frontier3.svg", width = 5, height = 3)

# play sound when finished
beep(sound = 2)
