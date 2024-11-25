test_that("parametric_bootstrap woorks", {
  fit <- simple_lm_fit()
  stat <- function(model) {
    mean(fitted(model))
  }
  withr::local_seed(1)
  result <- parametric_bootstrap(fit, stat, nsim = 10, simplify = FALSE)
  expect_length(result$result, 10)
  expect_type(result$result, "list")
  expect_type(result$responses, "list")
  expect_type(result$simulation_warning, "logical")
  expect_type(result$converged, "logical")

  sim_result <- Reduce(c, result$result)
  expect_gt(sd(sim_result), 0)
})

test_that("parametric_bootstrap simplify into vector or matrix", {
  fit <- simple_lm_fit()
  stat1 <- function(model) coef(model)[[1]]
  stat2 <- function(model) coef(model)[1:2]
  withr::local_seed(1)
  result1 <- parametric_bootstrap(fit, stat1, nsim = 10, simplify = TRUE)
  result2 <- parametric_bootstrap(fit, stat2, nsim = 10, simplify = TRUE)

  expect_vector(result1$result, ptype = double(), size = 10)
  expect_vector(result2$result,
    ptype = rbind(
      "(Intercept)" = numeric(10),
      "x" = numeric(10)
    ), size = 2L
  )
})

test_that("parametric_bootstrap returns correct length with errors within simulations", {
  foo <- expand.grid(a = factor(1:3), b = factor(1:3))
  foo$y <- 1
  fit <- glm(y ~ a + b, data = foo, family = poisson())
  responses <- data.frame(s1 = foo$y, s2 = c(-1, rep(0, 8)), s3 = foo$y)
  suppressWarnings(sim_res <- parametric_bootstrap(fit, statistic = coef, responses = responses))
  expect_length(sim_res$result, 3)
})

test_that("health_check works as expected", {
  expect_error(bootstrap_health_check(list(NULL), FALSE, FALSE), "Could not refit")
  expect_warning(bootstrap_health_check(list("foo", NULL), FALSE, TRUE), "1 statistics not computed")
  expect_warning(bootstrap_health_check(list("foo", "foo"), c(FALSE, TRUE), TRUE), "1 simulations threw warnings")
  expect_warning(bootstrap_health_check(list("foo", "foo"), c(FALSE, FALSE), c(FALSE, TRUE)), "1 simulations diverged")
})
