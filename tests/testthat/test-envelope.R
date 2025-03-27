test_that("envelope() works with responses", {
  fit <- glm(carb ~ cyl, data = mtcars, family = poisson())
  withr::local_seed(1)
  yyok <- simulate(fit, 5)
  yyerror <- as.data.frame(matrix(rnorm(nrow(mtcars) * 5), ncol = 5))
  expect_no_error(envelope(fit, responses = yyok, no_warnings = TRUE))
  expect_error(envelope(fit, responses = yyerror), "Could not refit")
})

test_that("envelope() observed and expected concides with qqnorm", {
  fit <- lm(mpg ~ cyl, data = mtcars)
  residual_fn <- stats::rstudent
  env_meas <- envelope(fit, residual_fn = residual_fn, nsim = 5, plot.it = FALSE)
  residual <- residual_fn(fit)
  expect_identical(env_meas$observed, residual)
  expect_identical(env_meas$observed, stats::qqnorm(residual, plot.it = FALSE)$y)
})

test_that("envelope intervals are correct", {
  fit <- simple_lm_fit()
  env_meas <- envelope(fit, residual_fn = residuals, nsim = 5, plot.it = FALSE)
  expect_true(all(env_meas$lower <= env_meas$med & env_meas$med <= env_meas$upper))
})

test_that("envelope() acceptable coverage for correct model", {
  withr::local_seed(1)
  x <- matrix(rnorm(1000), ncol = 2)
  beta <- c(1, 1)
  y <- rnorm(nrow(x), mean = x %*% beta, sd = 0.01)
  fit <- lm(y ~ x + 0)
  env_meas <- envelope(fit, nsim = 25, plot.it = TRUE)
  inside_band <- mean(env_meas$lower <= env_meas$observed & env_meas$observed <= env_meas$upper)
  expect_gte(inside_band, 0.87)
  expect_equal(1 - inside_band, mean(env_meas$outside))
})

test_that("envelope() detects incorrect fit", {
  withr::local_seed(1)
  x <- matrix(rnorm(1000), ncol = 2)
  beta <- c(1, 1)
  y <- rpois(nrow(x), lambda = exp(x %*% beta))
  fit <- lm(y ~ x + 0)
  env_meas <- envelope(fit, nsim = 10, plot.it = FALSE)
  outside_band <- mean(env_meas$outside)
  expect_gt(outside_band, 0.5)
})

test_that("plot envelope runs without errors", {
  expect_no_error({
    env_meas <- envelope(lm(c(1, 5) ~ 1), nsim = 2, residual_fn = residuals, plot.it = FALSE)
    plot(env_meas)
  })
})

test_that("envelope() is compatible with models using cbind", {
  m <- c(1, 4, 10, 30)
  y <- c(0, 2, 5, 15)
  fit <- glm(cbind(y, m - y) ~ 1, family = binomial())
  expect_no_error(envelope(fit, nsim = 2, plot.it = FALSE))
})

test_that("envelope works with lme4::lmer", {
  skip_on_cran()
  data("sleepstudy", package = "lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  expect_no_error(envelope(fit, nsim = 2, residual_fn = residuals, plot.it = FALSE))
})

test_that("envelope works with lme4::glmer", {
  skip_on_cran()
  data("cbpp", package = "lme4")
  fit <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp, family = binomial
  )
  expect_no_error(envelope(fit, nsim = 2, residual_fn = residuals, plot.it = FALSE))
})

test_that("envelope works with glmmTMB", {
  skip_on_cran()
  data("Salamanders", package = "glmmTMB")
  m1 <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    zi = ~mined,
    family = poisson, data = Salamanders
  )
  expect_no_error(envelope(m1, nsim = 2, residual_fn = residuals))
  m2 <- glmmTMB::glmmTMB(count ~ spp + mined + (1 | site),
    zi = ~ spp + mined,
    family = glmmTMB::nbinom2, data = Salamanders
  )
  expect_no_error(envelope(m2, nsim = 2, residual_fn = residuals))
})

test_that("envelope works with complex responses", {
  responses <- list(
    list(y = 1:3, n = 2:4)
  )
  refit_fn <- function(.l) {
    .y <- .l$y
    .n <- .l$n
    glm(.y ~ 1 + offset(log(.n)), family = poisson())
  }
  fit <- glm(1:3 ~ 1 + offset(2:4), family = poisson())
  expect_no_error(envelope(fit, responses = responses, refit_fn = refit_fn, plot.it = FALSE))
})

test_that("envelope works with errors within simulations", {
  foo <- expand.grid(a = factor(1:3), b = factor(1:3))
  foo$y <- 1
  fit <- glm(y ~ a + b, data = foo, family = poisson())
  responses <- data.frame(s1 = foo$y, s2 = c(-1, rep(0, 8)), s3 = foo$y)
  expect_no_error(sim_res <- envelope(fit, responses = responses)) |>
    suppressWarnings()
  expect_identical(sim_res$lower, sim_res$observed, ignore_attr = TRUE)
  expect_identical(sim_res$med, sim_res$observed, ignore_attr = TRUE)
  expect_identical(sim_res$upper, sim_res$observed, ignore_attr = TRUE)
})

test_that("envelope don't show condition messages when its supposed to be removed", {
  responses <- list(y = 1:3)
  fit <- glm(1:3 ~ 1 + offset(2:4), family = poisson())
  expect_warning(
    expect_warning(
      expect_no_error(
        envelope(fit, responses = responses, plot.it = FALSE, maxit = 1)
      ), "threw warning"
    ), "diverged"
  )
  expect_error(
    expect_warning(
      envelope(fit,
        responses = responses, plot.it = FALSE, maxit = 1,
        converged_only = TRUE
      ), "threw warning"
    )
  )
  expect_error(
    expect_warning(
      envelope(fit,
        responses = responses, plot.it = FALSE, maxit = 1,
        no_warnings = TRUE
      ), "diverged"
    )
  )
  expect_error(
    expect_no_warning(
      envelope(fit,
        responses = responses, plot.it = FALSE, maxit = 1,
        no_warnings = TRUE, converged_only = TRUE
      )
    )
  )
  withr::local_seed(1)
  envelope(simple_lm_fit(),
    nsim = 2, refit_fn = function(.y) msg_fit(.y), plot.it = FALSE
  ) |>
    expect_message("2 simulations shown messages")
  # It errors when remove all the simulations
  envelope(simple_lm_fit(),
    nsim = 2, refit_fn = function(.y) msg_fit(.y), no_messages = TRUE, plot.it = FALSE
  ) |>
    expect_message("2 simulations shown messages") |>
    expect_error()
})

test_that("compute values outside correctly", {
  expect_identical(is_outside(c(1, 2, 3), c(1, 0, 3.01), c(1, 1.9, 5)), c(FALSE, TRUE, TRUE))
})

test_that("envelope_residual extracts the correct method", {
  fit_lm <- simple_lm_fit()
  fit_poi <- simple_pois_fit()
  fit_bin <- simple_cbind_bin_fit()

  will_resid <- function(obj) {
    h <- hatvalues(obj)
    sqrt(
      rstandard(obj, type = "deviance")^2 * (1 - h) +
        rstandard(obj, type = "pearson")^2 * h
    )
  }

  expect_equal(envelope_residual(fit_lm)(fit_lm), abs(rstudent(fit_lm)))
  expect_equal(envelope_residual(fit_bin)(fit_bin), will_resid(fit_bin))
  expect_equal(envelope_residual(fit_poi)(fit_poi), will_resid(fit_poi))
  expect_equal(envelope_residual(fit_poi)(fit_bin), will_resid(fit_bin))
  expect_equal(envelope_residual(fit_poi)(fit_lm), abs(rstudent(fit_lm)))
})

test_that("can manipulate ... with envelope_residual", {
  fit_poi <- simple_pois_fit()
  # Shouldn't error, because it's only generating a residual function.
  expect_no_error(res_fn <- envelope_residual(fit_poi, infl = NULL))
  # When we apply the function it should throw an error.
  expect_error(res_fn(fit_poi))
})

test_that("envelope_residual works with envelope", {
  fit_lm <- simple_lm_fit()
  withr::local_seed(1)
  expect_no_error(envelope(fit_lm, nsim = 2, residual_fn = envelope_residual(fit_lm), plot.it = FALSE))
})
