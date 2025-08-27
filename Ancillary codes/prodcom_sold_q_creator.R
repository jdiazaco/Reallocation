rpareto_trunc <- function(n, xm, alpha, xmax, p_zero = 0.25) {
  # p_zero = probability mass at 0
  # xm = scale parameter
  # alpha = shape parameter
  # xmax = truncation point
  
  # First, decide which draws are zero
  is_zero <- rbinom(n, 1, p_zero)
  
  # For nonzero draws: sample from truncated Pareto
  u <- runif(sum(is_zero == 0))
  x <- xm / ( ( (1 - u) * (1 - (xm/xmax)^alpha) + (xm/xmax)^alpha )^(1/alpha) )
  
  # Fill in result
  result <- numeric(n)
  result[is_zero == 1] <- 0
  result[is_zero == 0] <- x
  return(result)
}

# Example usage
set.seed(123)
sim_data <- rpareto_trunc(n = nrow(product_data),
                          xm = 0.95,
                          alpha = 0.10,
                          xmax = 1.786e11,
                          p_zero = 0.25)