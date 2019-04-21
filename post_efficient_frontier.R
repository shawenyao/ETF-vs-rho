suppressWarnings(library(tidyverse))
suppressWarnings(library(beepr))

set.seed(350)

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho")
source("functions/functions_efficient_frontier.R", echo = FALSE)
source("functions/functions_save_svg.R", echo = TRUE)


#==== general setup ====
rf <- 0.01
miu <- c(0.05, 0.15, 0.16)
sigma <- c(0.20, 0.24, 0.28)
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
risk_free <- tibble(volatility = 0, return = rf)


#==== global minimum variance portfolio ====
weight_minimum <- calculate_minimum_variance_portfolio(miu, sigma, rho)
minimum_portfolio <- calculate_return_volatility(list(weight_minimum), miu, sigma, rho)


#==== tangency portfolio ====
weight_tangency <- calculate_tangency_portfolio(rf, miu, sigma, rho)
tangency_portfolio <- calculate_return_volatility(list(weight_tangency), miu, sigma, rho)


#==== efficient frontier ====
weights_efficient_frontier <- calculate_efficient_frontier(miu, sigma, rho, min_return = 0.001, max_return = 0.25, step = 0.005)
efficient_frontier <- calculate_return_volatility(weights_efficient_frontier, miu, sigma, rho)


#==== tangency line ====
tangency_line <- tibble(
  volatility = seq(from = 0, to = 0.365, by = 0.01)
) %>% 
  mutate(
    return = (tangency_portfolio$return - rf) / tangency_portfolio$volatility * volatility + rf
  )


#==== plot ====
plot1 <- ggplot(efficient_frontier, aes(x = volatility, y = return)) +
  # the efficient frontier
  geom_path(size = 2, linejoin = "round", lineend = "round", color = "gray66") +
  # the tangency line
  geom_path(data = tangency_line, linetype = "dashed", size = 1.5, lineend = "round", color = "gray85") +
  # the atomic assets
  geom_point(data = atomic_assets, color = "dodgerblue3", size = 5) +
  geom_point(data = atomic_assets, color = "white", size = 2) +
  # the global minimum variance portfolio
  geom_point(data = minimum_portfolio, color = "chartreuse4", size = 5) +
  geom_point(data = minimum_portfolio, color = "white", size = 2) +
  # the tangency portfolio
  geom_point(data = tangency_portfolio, color = "coral3", size = 5) +
  geom_point(data = tangency_portfolio, color = "white", size = 2) +
  # the risk-free rate
  geom_point(data = risk_free, color = "chocolate1", size = 5) +
  geom_point(data = risk_free, color = "white", size = 2) +
  xlim(0, max(efficient_frontier$volatility)) +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
  ) +
  labs(x = "Volatility", y = "Return")

save_svg(plot = plot1, file_name = "output/efficient_frontier1.svg", width = 5, height = 3)

# play sound when finished
beep(sound = 2)
