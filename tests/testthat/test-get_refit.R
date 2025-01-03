test_that("response is replaced when using c() as response", {
  fit <- lm(c(2, 4, 5) ~ c(3, 5, 7))
  model_refit <- refit_model(fit, c(1, 1, 1))
  expect_equal(get_model_response(model_refit), c(1, 1, 1))
})

test_that("response is replaced when using data", {
  df <- data.frame(y = c(2, 4, 5), x = c(1, 2, 3))
  fit <- lm(y ~ x, data = df)
  model_refit <- refit_model(fit, c(1, 1, 1))
  expect_equal(get_model_response(model_refit), c(1, 1, 1))
})

test_that("refit_safely works", {
  custom_refit <- function(y) {
    lm(y ~ c(3, 5, 7))
  }
  expect_equal(coef(refit_safely(custom_refit, c(1, 1, 1))$value), c("(Intercept)" = 1, "c(3, 5, 7)" = 0))
  expect_equal(coef(refit_safely(custom_refit, 1:3)$value), c("(Intercept)" = -0.5, "c(3, 5, 7)" = 0.5))
})

test_that("refit_safely can capture all conditional messages", {
  call_messages_error <- function(newresp = NULL, throw = TRUE, ...) {
    message("foo")
    warning("bar")
    if (throw) stop("baz")
    c(1, 2, 3)
  }
  # HACK: There is a \n in message
  expect_no_condition(refit_safely(call_messages_error, NULL, throw = TRUE)) |>
    expect_mapequal(list(value = NULL, message = "foo\n", warning = "bar", error = "baz"))
  expect_no_condition(refit_safely(call_messages_error, NULL, throw = FALSE)) |>
    expect_mapequal(list(value = c(1, 2, 3), message = "foo\n", warning = "bar", error = NULL))
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
  data <- cbind_data()
  fit <- simple_cbind_bin_fit()
  ystar <- simulate(fit, nsim = 1, seed = 1)[[1]]
  #  HACK: Add data = data because the test fails inside the test environment. Outside it runs fine.
  expect_no_error(new_fit <- update_using_formula(fit, ystar, data = data))
  expect_false(identical(coef(fit), coef(new_fit)))
  expect_identical(get_model_response(new_fit), ystar, ignore_attr = TRUE)
})

test_that("find_refit_fn returns the expected function", {
  lm_fit <- simple_lm_fit()
  y <- simple_y()
  expect_identical(find_refit_fn(lm_fit, y), update_using_model_frame)

  lmer_fit <- simple_lmer_fit()
  lmer_y <- lmer_data()$y
  expect_identical(find_refit_fn(lmer_fit, lmer_y), lme4::refit) |>
    suppressWarnings()

  # TODO: If there is a get_refit method for survival models, use it here
  survival_fit <- simple_surv_fit()
  survival_y <- surv_data()$time
  expect_null(find_refit_fn(survival_fit, survival_y))
})

test_that("Behavior when lme4 is unavailable using with_mocked_bindings", {
  # Mock the requireNamespace function to return FALSE for "lme4"
  with_mocked_bindings(
    requireNamespace = function(pkg, quietly = TRUE) {
      if (pkg == "lme4") {
        return(FALSE)
      } else {
        return(base::requireNamespace(pkg, quietly))
      }
    },
    {
      # Verify that the mock works
      expect_false(requireNamespace("lme4", quietly = TRUE))

      lm_fit <- simple_lm_fit()
      y <- simple_y()
      expect_identical(find_refit_fn(lm_fit, y), update_using_model_frame)

      lmer_fit <- simple_lmer_fit()
      lmer_y <- lmer_data()$y
      identical(find_refit_fn(lmer_fit, lmer_y), lme4::refit) |>
        expect_false() |>
        suppressWarnings()
    }
  )
  identical(find_refit_fn(lmer_fit, lmer_y), lme4::refit) |>
    expect_true() |>
    suppressWarnings()
})

test_that("Can use update the dots for refit funcitons", {
  pois_fit <- simple_pois_fit()
  pois_y <- count_data()$y
  model_refit <- refit_model(pois_fit, pois_y, family = gaussian())
  expect_identical(model_refit$family$family, "gaussian")

  ref_fn <- find_refit_fn(pois_fit, pois_y)
  model_refit2 <- ref_fn(pois_fit, pois_y, family = gaussian())
  expect_identical(model_refit, model_refit2)
})
