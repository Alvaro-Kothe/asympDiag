test_that("response is replaced when using c() as response", {
  fit <- lm(c(2, 4, 5) ~ c(3, 5, 7))
  model_refit <- refit_model(fit, c(1, 1, 1))
  expect_equal(stats::model.frame(model_refit)[[1]], c(1, 1, 1))
})

test_that("response is replaced when using data", {
  df <- data.frame(y = c(2, 4, 5), x = c(1, 2, 3))
  fit <- lm(y ~ x, data = df)
  model_refit <- refit_model(fit, c(1, 1, 1))
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

test_that("can refit for models adjusted with cbind", {
  data <- data.frame(
    trials = c(10, 10, 10, 10, 10),
    successes = c(4, 6, 7, 3, 5),
    group = factor(c("A", "A", "B", "B", "C"))
  )

  fit <- glm(cbind(successes, trials - successes) ~ group,
    data = data,
    family = binomial()
  )
  ystar <- simulate(fit, nsim = 1, seed = 1)[[1]]
  #  HACK: Add data = data because the test fails inside the test environment. Outside it runs fine.
  expect_no_error(new_fit <- update_using_formula(fit, ystar, data = data))
  expect_false(identical(coef(fit), coef(new_fit)))
  expect_identical(model.frame(new_fit)[[1]], ystar, ignore_attr = TRUE)
})

test_that("find_refit_fn returns the expected function", {
  lm_fit <- simple_lm_fit()
  y <- simple_y()
  expect_identical(find_refit_fn(lm_fit, y), update_using_model_frame)

  lmer_fit <- simple_lmer_fit()
  lmer_y <- lmer_data()$y
  expect_identical(find_refit_fn(lmer_fit, lmer_y), get_refit) |>
    suppressWarnings()

  # TODO: If there is a get_refit method for survival models, use it here
  survival_fit <- simple_surv_fit()
  survival_y <- surv_data()$time
  expect_null(find_refit_fn(survival_fit, survival_y))
})
