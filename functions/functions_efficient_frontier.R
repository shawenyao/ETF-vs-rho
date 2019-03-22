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
