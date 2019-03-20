#' find the weights of the tangent portfolio
#' 
#' @param rf risk-free rate
#' @param miu a vector of expected return
#' @param sigma a vector of volatility
#' @param rho a square matrix of correlation
#' 
#' @return a vector of weights
#' 
calcualte_tangent_portfolio <- function(rf, miu, sigma, rho){
  
  # the variance-covariance matrix
  omega <- diag(sigma) %*% rho %*% diag(sigma)
  # ensure the weights sum up to 1
  normalizing_factor <- matrix(1, nrow = 1, ncol = ncol(omega)) %*% 
    solve(omega) %*% 
    (miu - rf)
  # the weights of the tangent portflio
  weights <- solve(omega) %*% (miu - rf) / as.numeric(normalizing_factor)
  
  return(weights)
}


#' find the expected return of the portfolio
#' 
#' @param weight a vector of asset weights
#' @param miu a vector of expected return
#' 
#' @return the expected return of the portfolio
#' 
calculate_return <- function(weight, miu){
  
  # return = sum(weight_i * return_i)
  sum(miu * weight)
}


#' find the weights of the tangent portfolio
#' 
#' @param weight a vector of asset weights
#' @param sigma a vector of volatility
#' @param rho a square matrix of correlation
#' 
#' @return the volatility of the portfolio
#' 
calculate_volatility <- function(weight, sigma, rho){
  
  # the variance-covariance matrix
  omega <- diag(sigma) %*% rho %*% diag(sigma)
  
  # sigma = sqrt(t(weight) %*% omega %*% weight)
  sqrt(matrix(weight, nrow = 1) %*% omega %*% matrix(weight, ncol = 1))
}
