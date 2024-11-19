# R/visualization_functions.R

library(ggplot2)
library(gridExtra)
library(magrittr)

#' Visualize theoretical sampling distribution with CLT comparison
#' @param n_values Vector of sample sizes to compare
#' @param p1 True proportion for group 1
#' @param p2 True proportion for group 2
#' @param reps Number of simulations per sample size
#' @return ggplot object
plot_sampling_dist <- function(n_values, p1, p2, reps = 1000) {
  # Theoretical parameters
  true_diff <- p1 - p2
  
  results <- lapply(n_values, function(n) {
    sims <- repeated_sims(p1, p2, n, n, reps)
    # Calculate standardized differences
    sims$z_score <- (sims$diff - true_diff) / sims$se
    sims$n <- n
    sims
  })
  
  plot_data <- do.call(rbind, results)
  
  # Create plots
  p1 <- ggplot(plot_data, aes(x = diff, fill = factor(n))) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = true_diff, linetype = "dashed") +
    labs(x = "Difference in Proportions",
         y = "Density",
         fill = "Sample Size",
         title = "Raw Differences") +
    theme_minimal()
  
  # Add theoretical normal curves
  p2 <- ggplot(plot_data, aes(x = z_score, fill = factor(n))) +
    geom_density(alpha = 0.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                  color = "black", linetype = "dashed") +
    labs(x = "Standardized Difference",
         y = "Density",
         fill = "Sample Size",
         title = "Standardized Differences vs N(0,1)") +
    theme_minimal()
  
  # Return both plots arranged side by side
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

#' Visualize confidence interval coverage
#' @param p1 True proportion for group 1
#' @param p2 True proportion for group 2
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#' @param alpha Significance level
#' @param max_reps Maximum number of repetitions
#' @return ggplot object
plot_confidence_coverage <- function(p1, p2, n1, n2, alpha = 0.05, max_reps = 1000) {
  reps_seq <- seq(10, max_reps, by = 10)
  true_diff <- p1 - p2
  z_val <- qnorm(1 - alpha/2)
  
  coverage <- sapply(reps_seq, function(r) {
    sims <- repeated_sims(p1, p2, n1, n2, r)
    intervals <- with(sims, {
      data.frame(
        lower = diff - z_val * se,
        upper = diff + z_val * se
      )
    })
    mean(intervals$lower <= true_diff & true_diff <= intervals$upper)
  })
  
  # Remove pipe operator and use regular ggplot syntax
  df <- data.frame(reps = reps_seq, coverage = coverage)
  ggplot(df, aes(x = reps, y = coverage)) +
    geom_line() +
    geom_hline(yintercept = 1 - alpha, linetype = "dashed", color = "red") +
    labs(x = "Number of Repetitions",
         y = "Coverage Probability",
         title = "Confidence Interval Coverage") +
    theme_minimal()
}

#' Visualize permutation test results
#' @param data Data frame with 'group' and 'response' columns
#' @param reps Number of permutations
#' @return ggplot object
plot_permutation_test <- function(data, reps = 1000) {
  perm_results <- permutation_test(data, reps)
  
  data.frame(stat = perm_results$null_dist) %>%
    ggplot(aes(x = stat)) +
    geom_histogram(aes(y = ..density..), bins = 30) +
    geom_density() +
    geom_vline(xintercept = perm_results$obs_stat, 
               color = "red", linetype = "dashed") +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                  color = "blue", linetype = "dashed") +
    labs(x = "Test Statistic",
         y = "Density",
         title = "Permutation Test Distribution") +
    theme_minimal()
}

#' Compare bootstrap and theoretical standard errors and intervals
#' @param data Data frame with 'group' and 'response' columns
#' @param reps Number of bootstrap samples
#' @param conf_level Confidence level
#' @return List of ggplot objects
plot_bootstrap_comparison <- function(data, reps = 1000, conf_level = 0.95) {
  # Calculate theoretical values
  theo_result <- calc_prop_diff(data)
  alpha <- 1 - conf_level
  z_val <- qnorm(1 - alpha/2)
  
  # Theoretical CI
  theo_ci <- c(
    theo_result$diff - z_val * theo_result$se,
    theo_result$diff + z_val * theo_result$se
  )
  
  # Bootstrap samples and CI
  boot_samples <- bootstrap_samples(data, reps)
  boot_se <- sd(boot_samples)
  boot_ci <- quantile(boot_samples, probs = c(alpha/2, 1-alpha/2))
  
  # Create comparison data
  se_data <- data.frame(
    method = c("Theoretical", "Bootstrap"),
    se = c(theo_result$se, boot_se)
  )
  
  ci_data <- data.frame(
    method = rep(c("Theoretical", "Bootstrap"), each = 2),
    type = rep(c("Lower", "Upper"), 2),
    value = c(theo_ci, boot_ci)
  )
  
  # SE comparison plot
  p1 <- ggplot(se_data, aes(x = method, y = se)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    labs(x = "Method", y = "Standard Error",
         title = "Standard Error Comparison") +
    theme_minimal()
  
  # CI comparison plot
  p2 <- ggplot(ci_data, aes(x = method, y = value, color = type)) +
    geom_point(size = 3) +
    geom_line(aes(group = type)) +
    labs(x = "Method", y = "Confidence Interval Bound",
         title = "Confidence Interval Comparison") +
    theme_minimal()
  
  # Distribution comparison
  p3 <- ggplot() +
    geom_density(aes(x = boot_samples, fill = "Bootstrap"), alpha = 0.5) +
    stat_function(fun = dnorm, 
                  args = list(mean = theo_result$diff, 
                              sd = theo_result$se),
                  aes(color = "Theoretical")) +
    labs(x = "Difference in Proportions", y = "Density",
         title = "Distribution Comparison") +
    theme_minimal()
  
  gridExtra::grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)
}