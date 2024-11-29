test_that("seeded runs can be replicated", {
  fit <- simple_lm_fit()

  withr::local_seed(1)
  s11 <- simulate_wald_pvalues(fit, nsim = 5)
  s12 <- simulate_wald_pvalues(fit, nsim = 5)

  withr::local_seed(1)
  s21 <- simulate_wald_pvalues(fit, nsim = 5)
  s22 <- simulate_wald_pvalues(fit, nsim = 5)

  expect_identical(s11, s21)
  expect_identical(s12, s22)
  expect_false(identical(s11, s12))
})

test_that("test_coefficients generate different results", {
  fit <- simple_lm_fit()
  withr::local_seed(1)
  p_values_regular <- simulate_wald_pvalues(fit, nsim = 10)

  test_coefs <- c(1000000, 1000000)
  withr::local_seed(1)
  p_values_test_coefs <- simulate_wald_pvalues(fit, nsim = 10, test_coefficients = test_coefs)

  # Test if all elements are close to 0, as test_coefs were very high
  expect_true(all(p_values_test_coefs$pvalues_joint < 1e-8))
  expect_true(all(p_values_test_coefs$pvalues_matrix < 1e-8))
  expect_false(identical(p_values_regular$pvalues_joint, p_values_test_coefs$pvalues_joint))
  expect_named(p_values_regular$test_coefficients, c("(Intercept)", "x"))
  expect_identical(p_values_test_coefs$test_coefficients, c(par_1 = 1000000, par_2 = 1000000))
})

test_that("rownames matrix is the same as model's coefficients names", {
  df <- data.frame("foo" = c(1, 2, 3, 7), "bar" = c(2, 3, 1, 9), "y" = c(3, 5, 1, 2))
  fit <- lm(y ~ foo + bar, data = df)
  p_values <- simulate_wald_pvalues(fit, nsim = 2)
  expect_named(p_values$pvalues_matrix[, 1], c("(Intercept)", "foo", "bar"))
})

test_that("Same response yield same result", {
  fit <- simple_lm_fit()
  withr::local_seed(1)
  responses <- simulate(fit, 5)
  responses2 <- simulate(fit, 5)

  sim1 <- simulate_wald_pvalues(fit, nsim = 5, responses = responses)
  sim2 <- simulate_wald_pvalues(fit, nsim = 5, responses = responses)
  sim3 <- simulate_wald_pvalues(fit, nsim = 5, responses = responses2)

  expect_identical(sim1, sim2)
  expect_false(identical(sim1, sim3))
})

test_that("simulate_wald_pvalues is able to capture warnings and errors", {
  foo <- expand.grid(a = factor(1:3), b = factor(1:3))
  foo$y <- 1
  fit <- glm(y ~ a + b, data = foo, family = poisson())
  responses <- data.frame(s1 = foo$y, s2 = c(1, rep(0, 8)), s3 = c(-1, rep(0, 8)))
  suppressWarnings(expect_no_error(
    pv <- simulate_wald_pvalues(fit, responses = responses)
  ))
  expect_identical(pv$simulation_warning, c(FALSE, TRUE, FALSE))
  expect_identical(pv$pvalues_joint, c(1, 1, NA))
})

test_that("simulate_wald_pvalues throw warning with singular matrix", {
  params <- c(1, .2, .5, -.2, -.5, 0, 0.1, 0.01, 3, 5)
  withr::local_seed(1)
  model_matrix <- matrix(rnorm(13 * 10),
    nrow = 13,
    ncol = 10
  )
  eta <- model_matrix %*% params
  mu <- exp(eta)
  y <- rpois(13, mu)

  suppressWarnings({
    fit <- glm(y ~ model_matrix + 1, family = poisson())

    expect_warning(
      simulate_wald_pvalues(fit, nsim = 10, plot.it = FALSE),
      "Couldn't inverse vcov from \\d+ simulations."
    )
  })
})

test_that("simulate_wald_pvalues works with poisson with offset", {
  n <- 5
  offset_ <- rpois(n, 10000)
  x <- runif(n)
  y <- rpois(n, exp(1 + 2 * x))

  suppressWarnings({
    fit <- glm(y ~ x + offset(offset_), family = poisson())
    p_values <- simulate_wald_pvalues(fit, nsim = 5)
  })

  expect_length(p_values$simulation_fixef[[1]], 2)
})

test_that("simulate_wald_pvalues convergence for lm is NA", {
  fit <- simple_lm_fit()
  p_values <- simulate_wald_pvalues(fit, nsim = 2)
  expect_equal(p_values$converged, c(NA, NA))
})

test_that("simulate_wald_pvalues convergence for glm is logical", {
  fit <- glm(c(1, 3, 5) ~ c(1, 2, 3), family = poisson())
  withr::local_seed(1)
  p_values <- simulate_wald_pvalues(fit, nsim = 2)
  expect_equal(p_values$converged, c(TRUE, TRUE))
})

test_that("Can compute p_values from merMod class", {
  data("sleepstudy", package = "lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  withr::local_seed(1)
  expect_no_error(p_values <- simulate_wald_pvalues(fit, nsim = 2))
})

test_that("can compute p_values from glmmTMB", {
  data("Salamanders", package = "glmmTMB")
  m1 <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    zi = ~mined,
    family = poisson, data = Salamanders
  )
  expect_no_error(simulate_wald_pvalues(m1, nsim = 2))
})

test_that("Can compute p_values using other vcov functions", {
  fit <- glm(c(1, 3, 5) ~ c(1, 2, 3), family = poisson())
  withr::local_seed(1)
  new_resp <- simulate(fit, nsim = 2, seed = 1)
  p_values_sandwich <- simulate_wald_pvalues(
    fit,
    nsim = 2, responses = new_resp,
    vcov_fn = sandwich::sandwich
  )
  p_values_implicit <- simulate_wald_pvalues(
    fit,
    nsim = 2, responses = new_resp
  )
  p_values_explicit <- simulate_wald_pvalues(
    fit,
    nsim = 2, responses = new_resp,
    vcov_fn = get_vcov
  )
  expect_identical(p_values_implicit, p_values_explicit)
  expect_true(all(p_values_implicit$pvalues_matrix != p_values_sandwich$pvalues_matrix))
  expect_true(all(p_values_implicit$pvalues_joint != p_values_sandwich$pvalues_joint))
})

test_that("Can compute p_values from parameters outside of coef()", {
  suppressWarnings(fit <- MASS::glm.nb(c(4, 0, 7) ~ c(1, 2, 3), maxit = 5))
  get_coef_ <- function(object) {
    c(coef(object), object$theta)
  }
  get_vcov_ <- function(object) {
    as.matrix(Matrix::bdiag(vcov(object), object$SE.theta))
  }
  withr::local_seed(1)
  expect_no_error(
    suppressWarnings(
      simulate_wald_pvalues(fit, nsim = 2, vcov_fn = get_vcov_, coef_fn = get_coef_)
    )
  )
  expect_error(
    suppressWarnings(
      simulate_wald_pvalues(fit, nsim = 2, vcov_fn = vcov, coef_fn = get_coef_)
    )
  )
  expect_error(
    suppressWarnings(
      simulate_wald_pvalues(fit, nsim = 2, vcov_fn = get_vcov_, coef_fn = coef)
    )
  )
  expect_error(
    suppressWarnings(
      simulate_wald_pvalues(fit,
        nsim = 2, vcov_fn = get_vcov_, coef_fn = get_coef_,
        test_coefficients = coef(fit)
      )
    )
  )
})

test_that("simulate_wald_pvalues() with custom method works", {
  foo <- function(formula, data) {
    model_frame <- model.frame(formula, data = data)
    y <- model.response(model_frame, type = "numeric")
    x <- model.matrix(formula, data)
    xtx <- crossprod(x)
    xtxinv <- solve(xtx)

    out <- list()
    out$coef <- c(xtxinv %*% t(x) %*% y)
    out$fitted.values <- c(x %*% out$coef)
    out$sigma <- sum((y - out$fitted.values)^2) / (length(y) - length(out$coef))
    out$x <- x
    out$y <- y
    out$data <- data
    out$model_frame <- model_frame
    out$vcov <- out$sigma * xtxinv
    out$formula <- formula

    class(out) <- "foo"

    return(out)
  }

  rlang::local_bindings(
    simulate.foo = function(object, nsim = 1, seed = NULL, ...) {
      mu <- object$fitted.values
      replicate(nsim, rep(NA, length(mu)), simplify = FALSE)
    },
    vcov.foo = function(object, ...) matrix(NA, nrow = 2, ncol = 2),
    coef.foo = function(object, ...) rep_len(NA, 2),
    get_refit.foo = function(object, newresp, ...) object,
    .env = globalenv()
  )

  fit <- foo(mpg ~ cyl, mtcars)

  # Fail because there should not be valid p-values
  suppressWarnings(expect_error(simulate_wald_pvalues(fit, nsim = 2, plot.it = TRUE)))
  suppressWarnings(expect_no_error(sim <- simulate_wald_pvalues(fit, nsim = 2, plot.it = FALSE)))

  expect_identical(sim$simulation_fixef[[1]], c(NA, NA))

  expect_identical(
    sim$simulation_vcov[[1]],
    matrix(NA, nrow = 2, ncol = 2)
  )
})

test_that("pvalues health check", {
  fine <- list(joint = c(1, 1, 1))
  expect_no_condition(p_values_health_check(fine))
  sing <- list(joint = c(NA, 1, NA))
  expect_warning(p_values_health_check(sing), "Couldn't inverse vcov from 2 simulations")
})

test_that("plot_pvalues errors when there is no available p-value to plot", {
  fit <- simple_lm_fit()
  p_values <- simulate_wald_pvalues(fit, nsim = 5)
  p_values$converged <- c(FALSE, FALSE, FALSE, TRUE, TRUE)
  p_values$pvalues_joint[4:5] <- NA_real_
  expect_no_error(plot(p_values, ask = FALSE, converged_only = FALSE))
  expect_error(
    plot(p_values, ask = FALSE, converged_only = TRUE),
    "^No p-value to plot\\.$"
  )
})

test_that("plot pvalues works with captions as a character vector", {
  fit <- simple_lm_fit()
  p_values <- simulate_wald_pvalues(fit, nsim = 3, plot.it = FALSE)
  expect_no_error(plot(p_values, which = 3, caption = "foo"))
  expect_no_error(plot(p_values, which = 99999999, caption = "foo"))
  expect_no_error(plot(p_values, which = Inf, caption = "foo"))
  expect_no_error(plot(p_values, which = 2:3, caption = c("a", "b")))
  expect_no_error(plot(p_values, which = 1:3, caption = "foo"))
})

test_that("plot pvalues works when the plot captions defined for the coefficient index", {
  fit <- simple_lm_fit()
  p_values <- simulate_wald_pvalues(fit, nsim = 3, plot.it = FALSE)
  expect_error(plot(p_values, which = 2:3, caption = list("a", "b")))
  expect_no_error(plot(p_values, which = 2:3, caption = list("a", "b", "c")))
  expect_no_error(plot(p_values, which = 1:2, caption = list("a", "b")))
})
