#' find the expected return of the portfolio
#' 
#' @param weight a vector of asset weights
#' @param miu a vector of expected return
#' 
#' @return the expected return of the portfolio
#' 
calculate_return <- function(weight, miu){
  
  # return = sum(weight_i * return_i)
  sum(weight * miu)
}


#' find the volatility of the portfolio
#' 
#' @param weight a vector of asset weights
#' @param sigma a vector of volatility
#' @param rho a square matrix of correlation
#' 
#' @return the volatility of the portfolio
#' 
calculate_volatility <- function(weight, sigma, rho){
  
  # the variance-covariance matrix
  big_sigma <- diag(sigma) %*% rho %*% diag(sigma)
  
  # volatility = sqrt(t(weight) %*% big_sigma %*% weight)
  as.numeric(sqrt(matrix(weight, nrow = 1) %*% big_sigma %*% matrix(weight, ncol = 1)))
}


#' find the return and volatility given a list of asset weights
#' 
#' @param weights a list of vector of weights
#' @param miu a vector of expected return
#' @param sigma a vector of volatility
#' @param rho a square matrix of correlation
#' 
#' @return a data.frame of expected return and volatility
#' 
calculate_return_volatility <- function(weights, miu, sigma, rho){
  
  # find the return and volatility for each set of portfolio weights
  tibble(
    return = weights %>% 
      map(calculate_return, miu = miu) %>% 
      unlist(),
    volatility = weights %>% 
      map(calculate_volatility, sigma = sigma, rho = rho) %>% 
      unlist()
  )
}


#' find the global minimum variance portfolio
#' 
#' @param miu a vector of expected return
#' @param sigma a vector of volatility
#' @param rho a square matrix of correlation
#' 
#' @return a list of the vector of weights of the global minimum variance portfolio
#' 
calculate_minimum_variance_portfolio <- function(miu, sigma, rho){
  
  # the variance-covariance matrix
  big_sigma <- diag(sigma) %*% rho %*% diag(sigma)

  # solving for a %*% x = b
  a <- (big_sigma * 2) %>%
    cbind(matrix(1, nrow = 3, ncol = 1)) %>% 
    rbind(matrix(c(rep(1, times = length(miu)), 0), nrow = 1))
  b <- matrix(c(rep(0, times = length(miu)), 1), ncol = 1)
  
  # return the first N elements of the solution (the weights of the portfolio)
  solve(a, b)[seq_along(miu),1]
}


#' find the tangency portfolio
#' 
#' @param rf risk-free rate
#' @param miu a vector of expected return
#' @param sigma a vector of volatility
#' @param rho a square matrix of correlation
#' 
#' @return a list of the vector of weights of the tangency portfolio
#' 
calculate_tangency_portfolio <- function(rf, miu, sigma, rho){
  
  # the variance-covariance matrix
  big_sigma <- diag(sigma) %*% rho %*% diag(sigma)
  
  # ensure the weights sum up to 1
  normalizing_factor <- matrix(1, nrow = 1, ncol = ncol(big_sigma)) %*% 
    solve(big_sigma) %*% 
    (miu - rf)
  
  # the weights of the tangency portflio
  solve(big_sigma) %*% (miu - rf) / as.numeric(normalizing_factor)
}


#' find the portfolio with the lowest volatility given a target expected return
#' 
#' @param miu a vector of expected return
#' @param sigma a vector of volatility
#' @param rho a square matrix of correlation
#' @param target_return the desired level of return
#' 
#' @return a vector of the asset weights in the desired portfolio
#' 
calculate_efficient_portfolio <- function(miu, sigma, rho, target_return){
  
  # the variance-covariance matrix
  big_sigma <- diag(sigma) %*% rho %*% diag(sigma)
  
  # solving for a %*% x = b
  a <- big_sigma %>% 
    cbind(matrix(miu, ncol = 1), 1) %>% 
    rbind(matrix(c(miu, 0, 0), nrow = 1)) %>% 
    rbind(matrix(c(rep(1, times = length(miu)), 0, 0), nrow = 1))
  b <- matrix(c(rep(0, times = length(miu)), target_return, 1), ncol = 1)
  
  # return the first N elements of the solution (the weights of the portfolio)
  solve(a, b)[seq_along(miu),1]
}


#' find the efficient frontier
#' 
#' @param miu a vector of expected return
#' @param sigma a vector of volatility
#' @param rho a square matrix of correlation
#' @param min_return the minimum return to be considered
#' @param max_return the maximum return to be considered
#' @param step the size of each step
#' 
#' @return a list of weights along the efficient frontier
#' 
calculate_efficient_frontier <- function(miu, sigma, rho, min_return, max_return, step){
  
  # scan over the target return range
  # and find the portfolio with minimal volatility given a target expected return 
  seq(min_return, max_return, by = step) %>% 
    map(calculate_efficient_portfolio, miu = miu, sigma = sigma, rho = rho)
}


#' calculate and plot the efficeint frontier
#'
#' @param risky_assets a data.frame of the return and volatility of all risky assets
#' @param rho the correlation matrix of all risky assets
#' @param risk_free_asset a data.frame of the return and volatility (0) of risk-free asset
#' @param x_range the range of the x axis
#' @param y_range the range of the y axis
#' @param show_tangency_line a boolean switch to add tangency line to the plot
#' @param show_tangency_portfolio a boolean switch to add tangency portfolio to the plot
#'
#' @return a ggplot2 object
#'
plot_efficient_frontier <- function(
  risky_assets, 
  rho, 
  risk_free_asset,
  x_range,
  y_range,
  show_tangency_line = TRUE,
  show_tangency_portfolio = TRUE
){
  
  # global minimum variance portfolio
  weight_minimum <- calculate_minimum_variance_portfolio(
    miu = risky_assets$return,
    sigma = risky_assets$volatility,
    rho = rho
  )
  minimum_portfolio <- calculate_return_volatility(
    weights = list(weight_minimum), 
    miu = risky_assets$return,
    sigma = risky_assets$volatility,
    rho = rho
  )
  
  # tangency portfolio
  weight_tangency <- calculate_tangency_portfolio(
    rf = risk_free_asset$return,
    miu = risky_assets$return, 
    sigma = risky_assets$volatility, 
    rho = rho
  )
  tangency_portfolio <- calculate_return_volatility(
    weights = list(weight_tangency),
    miu = risky_assets$return,
    sigma = risky_assets$volatility,
    rho = rho
  )
  
  # efficient frontier
  weights_efficient_frontier <- calculate_efficient_frontier(
    miu = risky_assets$return,
    sigma = risky_assets$volatility,
    rho = rho,
    min_return = 0.001, 
    max_return = 0.25, 
    step = 0.005
  )
  efficient_frontier <- calculate_return_volatility(
    weights = weights_efficient_frontier,
    miu = risky_assets$return, 
    sigma = risky_assets$volatility, 
    rho = rho
  )
  
  # tangency line
  tangency_line <- tibble(
    volatility = seq(from = 0, to = 0.365, by = 0.01)
  ) %>% 
    mutate(
      return = (tangency_portfolio$return - risk_free_asset$return) /
        tangency_portfolio$volatility * volatility + 
        risk_free_asset$return
    )
  
  # return the plot
  output <- ggplot(efficient_frontier, aes(x = volatility, y = return)) +
    # the efficient frontier
    geom_path(size = 2, linejoin = "round", lineend = "round", color = "gray66") +
    # the atomic assets
    geom_point(data = risky_assets, color = "dodgerblue3", size = 5) +
    geom_point(data = risky_assets, color = "white", size = 2) +
    # the global minimum variance portfolio
    geom_point(data = minimum_portfolio, color = "chartreuse4", size = 5) +
    geom_point(data = minimum_portfolio, color = "white", size = 2) +
    # the risk-free rate
    geom_point(data = risk_free_asset, color = "chocolate1", size = 5) +
    geom_point(data = risk_free_asset, color = "white", size = 2) +
    xlim(x_range[1], x_range[2]) +
    ylim(y_range[1], y_range[2]) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
    ) +
    labs(x = "Volatility", y = "Return")
  
  if(isTRUE(show_tangency_line)){
    output <- output + 
      # the tangency line
      geom_path(
        data = tangency_line, 
        linetype = "dashed", 
        size = 1.5, 
        lineend = "round", 
        color = "gray85"
      )
  }
  
  if(isTRUE(show_tangency_portfolio)){
    output <- output + 
      # the tangency portfolio
      geom_point(data = tangency_portfolio, color = "coral3", size = 5) +
      geom_point(data = tangency_portfolio, color = "white", size = 2)
  }
  
  output
}
