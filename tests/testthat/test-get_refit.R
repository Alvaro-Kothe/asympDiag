test_that("response is replaced when using c() as response", {
  fit <- lm(c(2, 4, 5) ~ c(3, 5, 7))
  model_refit <- get_refit(fit, c(1, 1, 1))
  expect_equal(stats::model.frame(model_refit)[[1]], c(1, 1, 1))
})

test_that("response is replaced when using data", {
  df <- data.frame(y = c(2, 4, 5), x = c(1, 2, 3))
  fit <- lm(y ~ x, data = df)
  model_refit <- get_refit(fit, c(1, 1, 1))
  expect_equal(stats::model.frame(model_refit)[[1]], c(1, 1, 1))
})

test_that("refit_safely works", {
  custom_refit <- function(y) {
    lm(y ~ c(3, 5, 7))
  }
  expect_equal(coef(refit_safely(custom_refit, c(1, 1, 1))$value), c("(Intercept)" = 1, "c(3, 5, 7)" = 0))
  expect_equal(coef(refit_safely(custom_refit, 1:3)$value), c("(Intercept)" = -0.5, "c(3, 5, 7)" = 0.5))
})

test_that("default_refit_fn yield the same model as get_refit", {
  my_fit_fn <- function(y) {
    lm(y ~ c(1, 2, 3))
  }
  fit_diff <- function(y) {
    glm(y ~ c(1, 2, 3), family = poisson())
  }
  fit <- lm(c(-1, 0, 1) ~ c(1, 2, 3))

  my_fit <- default_refit_fn(my_fit_fn, fit)
  default <- default_refit_fn(NULL, fit)
  fi_diff <- default_refit_fn(fit_diff, fit)

  expect_error(default_refit_fn("foo", fit), "refit_fn should be a function")
  expect_equal(coef(my_fit(c(1, 1, 1))), coef(default(c(1, 1, 1))))
  expect_equal(
    coef(fit_diff(c(10, 20, 30))),
    c(`(Intercept)` = 1.86185714928969, `c(1, 2, 3)` = 0.522442285285042)
  )
})
