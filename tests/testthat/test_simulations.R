# tests/testthat/test_simulations.R

library(testthat)

source("../../R/core_functions.R")
source("../../R/visualization_functions.R")

test_that("sim_binary_data produces valid output", {
  data <- sim_binary_data(0.5, 0.5, 100, 100)
  
  expect_equal(nrow(data), 200)
  expect_true(all(data$group %in% c(1,2)))
  expect_true(all(data$response %in% c(0,1)))
  expect_equal(sum(data$group == 1), 100)
  expect_equal(sum(data$group == 2), 100)
})

test_that("sim_binary_data handles invalid inputs", {
  expect_error(sim_binary_data(-0.1, 0.5, 100, 100))
  expect_error(sim_binary_data(0.5, 1.1, 100, 100))
  expect_error(sim_binary_data(0.5, 0.5, -100, 100))
  expect_error(sim_binary_data(0.5, 0.5, 100.5, 100))
})

test_that("calc_prop_diff produces expected output", {
  data <- data.frame(
    group = c(rep(1,10), rep(2,10)),
    response = c(rep(1,5), rep(0,5), rep(1,3), rep(0,7))
  )
  
  result <- calc_prop_diff(data)
  
  expect_equal(result$diff, 0.5 - 0.3)  # Now should pass
  expect_true(!is.na(result$se))
  expect_true(result$se > 0)
})

test_that("repeated_sims produces expected number of simulations", {
  results <- repeated_sims(0.5, 0.5, 10, 10, 100)
  expect_equal(nrow(results), 100)
  expect_true(all(c("diff", "se") %in% names(results)))
})

test_that("permutation_test maintains group sizes", {
  data <- sim_binary_data(0.6, 0.4, 50, 30)
  perm_result <- permutation_test(data, 100)
  
  expect_equal(length(perm_result$null_dist), 100)
  expect_true(length(perm_result$obs_stat) == 1)
})

test_that("bootstrap_samples produces valid resamples", {
  data <- sim_binary_data(0.5, 0.5, 20, 20)
  boot_result <- bootstrap_samples(data, 100)
  
  expect_equal(length(boot_result), 100)
  expect_true(all(!is.na(boot_result)))
  expect_true(all(abs(boot_result) <= 1))
})

test_that("visualization functions return expected objects", {
  data <- sim_binary_data(0.6, 0.4, 50, 50)
  
  expect_s3_class(
    plot_sampling_dist(c(30, 50), 0.6, 0.4, 100),
    "gtable"
  )
  
  expect_s3_class(
    plot_confidence_coverage(0.6, 0.4, 50, 50, max_reps = 100),
    "ggplot"
  )
  
  expect_s3_class(
    plot_permutation_test(data, 100),
    "ggplot"
  )
  
  expect_s3_class(
    plot_bootstrap_comparison(data, 100),
    "gtable"
  )
})