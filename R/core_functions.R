# R/core_functions.R

#' Simulate binary responses for two groups
#' @param p1 Probability of success for group 1
#' @param p2 Probability of success for group 2
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#' @return Data frame with columns 'group' and 'response'
sim_binary_data <- function(p1, p2, n1, n2) {
  if (!all(c(p1, p2) >= 0 & c(p1, p2) <= 1)) {
    stop("Probabilities must be between 0 and 1")
  }
  if (!all(c(n1, n2) > 0 & c(n1, n2) %% 1 == 0)) {
    stop("Sample sizes must be positive integers")
  }
  
  data.frame(
    group = rep(1:2, times = c(n1, n2)),
    response = c(rbinom(n1, 1, p1), rbinom(n2, 1, p2))
  )
}

#' Calculate difference in proportions between two groups
#' @param data Data frame containing 'group' and 'response' columns
#' @return List containing the difference in proportions and standard error
calc_prop_diff <- function(data) {
  # Validate input data has required columns
  if (!all(c("group", "response") %in% names(data))) {
    stop("Data must contain 'group' and 'response' columns")
  }
  
  # Calculate proportions for each group
  group1_data <- data[data$group == 1, ]
  group2_data <- data[data$group == 2, ]
  
  p1 <- mean(group1_data$response)
  p2 <- mean(group2_data$response)
  
  # Calculate sample sizes
  n1 <- nrow(group1_data)
  n2 <- nrow(group2_data)
  
  # Calculate difference
  diff <- p1 - p2
  
  # Calculate standard error
  se <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
  
  # Return named list
  return(list(
    diff = diff,
    se = se,
    p1 = p1,
    p2 = p2,
    n1 = n1,
    n2 = n2
  ))
}
#' Perform repeated simulations
#' @param p1 Probability of success for group 1
#' @param p2 Probability of success for group 2
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#' @param reps Number of repetitions
#' @return Data frame with simulation results
repeated_sims <- function(p1, p2, n1, n2, reps) {
  results <- replicate(reps, {
    data <- sim_binary_data(p1, p2, n1, n2)
    calc_prop_diff(data)
  }, simplify = FALSE)
  
  data.frame(
    diff = sapply(results, function(x) x$diff),
    se = sapply(results, function(x) x$se)
  )
}

#' Perform permutation test
#' @param data Data frame with 'group' and 'response' columns
#' @param reps Number of permutations
#' @return List with null distribution and observed statistic
permutation_test <- function(data, reps) {
  obs_stat <- with(calc_prop_diff(data), diff/se)
  
  null_stats <- replicate(reps, {
    data$group <- sample(data$group)
    with(calc_prop_diff(data), diff/se)
  })
  
  list(
    null_dist = null_stats,
    obs_stat = obs_stat
  )
}

#' Perform bootstrap resampling
#' @param data Data frame with 'group' and 'response' columns
#' @param reps Number of bootstrap samples
#' @return Vector of bootstrap statistics
bootstrap_samples <- function(data, reps) {
  replicate(reps, {
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    calc_prop_diff(boot_data)$diff
  })
}