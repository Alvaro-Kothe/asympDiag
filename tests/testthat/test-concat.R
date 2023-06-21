sim_cyl <- function() {
  fit <- lm(mpg ~ cyl, data = mtcars)
  simulate_wald_pvalues(fit, nsim = 2)
}

test_that("Concatenate different models errors", {
  p_val_cyl <- sim_cyl()
  fit2 <- lm(mpg ~ vs, data = mtcars)
  p_val_vs <- simulate_wald_pvalues(fit2, nsim = 1)
  expect_error(concat_ld_pvalues(p_val_cyl, p_val_vs))
})

test_that("Concatenate increases p_values and simulations length", {
  sims <- list(sim_cyl(), sim_cyl(), sim_cyl())
  concatenated <- concat_pvalues(sims)
  expect_length(concatenated$test_coefficients, 2)
  expect_identical(dim(concatenated$pvalues_matrix), c(2L, 6L))
  expect_length(concatenated$pvalues_joint, 6)
  expect_length(concatenated$simulation_fixef, 6)
  expect_length(concatenated$simulation_vcov, 6)
  expect_length(concatenated$converged, 6)
  expect_length(concatenated$responses, 6)
})

test_that("Concatenates with list of size 1 returns same object", {
  sims <- list(sim_cyl())
  expect_identical(concat_pvalues(sims), sims[[1]])
})

test_that("concatenates errors with empty list", {
  expect_error(concat_pvalues(list()))
})
