#' Generate Simulated Envelope
#'
#' Generates QQ-plot with simulated envelope residuals.
#'
#' Simulates new responses using [stats::simulate()] and refits the model
#' for each vector of new responses using [get_refit()]. The function then computes
#' residuals for each simulation, sorts them, and constructs envelope bands and
#' a median line based on the quantiles of these residuals.
#'
#' `refit_fn` is a function that supposedly compute the refit of `model`.
#' Use this method if the default [get_refit()] doesn't work.
#' If `refit_fn` is `NULL`, it's value is defined as `function(y, ...) get_refit(model, y, ...)`.
#'
#' @param model A model to generate responses and compute the observed residuals.
#' @param residual_fn A function to calculate model residuals. The default is
#'   [envel_resid()] for an absolute residual.
#' @param alpha The significance level for constructing the envelope bounds.
#'   Defaults to 0.05.
#' @param nsim The number of simulations to perform for envelope construction.
#'   Defaults to 100.
#' @inheritParams parametric_bootstrap
#' @param no_warnings If TRUE, ignore simulations that threw warnings.
#' @param converged_only If TRUE, ignore simulations that diverged.
#' @param plot.it Logical. Generate envelope plot.
#' @param ... Extra arguments to [get_refit()]
#'
#' @return An object of class `AD_envelope`, which contains the following components:
#' \describe{
#'   \item{expected}{A vector of expected quantiles from a normal distribution.}
#'   \item{observed}{A vector of observed quantiles from the model residuals.}
#'   \item{outside}{A logical vector indicating whether each observation falls
#'   outside the constructed envelope bounds.}
#'   \item{lower}{The lower bounds of the envelope for each observation.}
#'   \item{med}{The median bounds of the envelope for each observation.}
#'   \item{upper}{The upper bounds of the envelope for each observation.}
#' }
#'
#' @examples
#' fit <- lm(mpg ~ cyl, data = mtcars)
#'
#' envelope(fit)
#'
#' # Use pearson residuals, and plot it agains the expected normal quantiles.
#' env_measures <- envelope(fit,
#'   residual_fn = function(x) residuals.lm(x, type = "pearson"), plot.it = FALSE
#' )
#' plot(env_measures, distribution = stats::qnorm, colors = c("gray", "black"))
#'
#' ## Using custom refit_fn
#' if (require("survival")) {
#'   fit <- survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian,
#'     dist = "exponential"
#'   )
#'   fitted_rate <- 1 / fitted(fit)
#'   new_responses <- replicate(100, rexp(length(fitted_rate), fitted_rate), simplify = FALSE)
#'   refit_surv_ovarian <- function(.y) {
#'     survreg(Surv(.y, fustat) ~ ecog.ps + rx, ovarian, dist = "exponential")
#'   }
#'   env_measures <- envelope(fit,
#'     responses = new_responses,
#'     residual_fn = function(x) abs(residuals(x, type = "deviance")),
#'     refit_fn = refit_surv_ovarian, plot.it = FALSE
#'   )
#'   # Absolute residuals are best shown with halfnormal quantiles
#'   plot(env_measures, distribution = function(p) qnorm((1 + p) / 2))
#' }
#'
#' @seealso \code{\link{get_refit}}, \code{\link{simulate}}, \code{\link{rstudent}}, \code{\link{plot.AD_envelope}},
#'  [parametric_bootstrap()]
#'
#' @export
envelope <- function(model, residual_fn = envelope_residual(model),
                     alpha = .05, nsim = 100,
                     responses = NULL,
                     no_warnings = FALSE,
                     converged_only = FALSE,
                     show_progress = TRUE,
                     plot.it = TRUE,
                     refit_fn = NULL,
                     ...) {
  obs_res <- residual_fn(model)
  sim_result <- parametric_bootstrap(
    model = model, statistic = function(x) sort(residual_fn(x), na.last = TRUE),
    nsim = nsim, refit_fn = refit_fn, responses = responses,
    simplify = FALSE, show_progress = show_progress,
    stat_hc = function(x) envelope_hc(x, length(obs_res)),
    show_warning_count = !no_warnings,
    show_not_converged_count = !converged_only,
    ...
  )
  simulation_residuals <- matrix(NA_real_, nrow = length(obs_res), ncol = length(sim_result$result))
  for (i in seq_along(sim_result$result)) {
    if (!is.null(sim_result$result[[i]])) {
      simulation_residuals[, i] <- sim_result$result[[i]]
    }
  }

  mask <- rep(TRUE, ncol(simulation_residuals))
  if (no_warnings) {
    mask <- mask & !sim_result$simulation_warning
  }
  if (converged_only) {
    mask <- mask & with(sim_result, is.na(converged) | converged)
  }
  simulation_residuals <- simulation_residuals[, mask, drop = FALSE]

  residual_quantiles <- apply(simulation_residuals, 1,
    stats::quantile,
    probs = c(alpha / 2, .5, 1 - alpha / 2), na.rm = TRUE
  )
  if (anyNA(residual_quantiles)) {
    stop("Could not compute the residuals of some models, verify the adjusted model and the `residual_fn`.")
  }

  ord_ord <- order(order(obs_res))
  lower <- residual_quantiles[1, ord_ord]
  med <- residual_quantiles[2, ord_ord]
  upper <- residual_quantiles[3, ord_ord]

  result <- list(
    observed = obs_res,
    outside = is_outside(obs_res, lower, upper),
    lower = lower, med = med, upper = upper
  )

  class(result) <- "AD_envelope"
  if (plot.it) {
    plot(result)
    return(invisible(result))
  }
  result
}

is_outside <- function(x, lower, upper) {
  x < lower | x > upper
}

envelope_hc <- function(stat, nobs) {
  if (any(!is.finite(stat))) {
    stop("`residual_fn` generated non-finite values.")
  }

  if ((ls <- length(stat)) != nobs) {
    stop("residuals from simulation has length ", ls, " and the model has length ", nobs, ".")
  }
}

#' Recommended Residuals for Envelope Plots
#'
#' This function returns a function that computes residuals for envelope plots.
#' These residuals are typically absolute values to be compared against the half-normal distribution.
#'
#' For objects of class `glm`, the default residuals are:
#' - Deviance residuals, except for `poisson` and `binomial` families.
#' - For `poisson` and `binomial` families, the residuals are [rstudent()], for the case deletion residual.
#'
#' For objects of class `lm`, the default residuals are also [rstudent()].
#'
#' For other classes, the default is [stats::residuals()], meaning no specialized recommendation is currently provided.
#'
#' @param object An object for which model residuals can be extracted.
#' @param ... Additional arguments passed to the residual function.
#'
#' @return A function that computes residuals from an object
#'
#' @export
envelope_residual <- function(object, ...) {
  UseMethod("envelope_residual")
}

#' @rdname envelope_residual
#' @export
envelope_residual.default <- function(object, ...) {
  function(obj) abs(stats::residuals(obj, ...))
}

#' @rdname envelope_residual
#' @export
envelope_residual.glm <- function(object, ...) {
  switch(object$family$family,
    binomial = ,
    poisson = function(obj) abs(stats::rstudent(obj, ...)),
    function(obj) abs(stats::residuals.glm(obj, type = "deviance", ...))
  )
}

#' @rdname envelope_residual
#' @export
envelope_residual.lm <- function(object, ...) {
  function(obj) abs(stats::rstudent(obj, ...))
}

#' Recommended Residual for Envelope
#'
#' This function retrieves a function for recommended residuals for envelope.
#' `envel_resid()` has been superseded in favour of [envelope_residual()].
#'
#' The residuals are absolute residuals to be plotted against the half-normal distribution.
#' For class `glm` the default is deviance, except for the families poisson and binomial,
#' where the default residual is [rstudent()], for deletion residuals.
#' For class `lm`, the default is also [rstudent()].
#' The default for other classes are [stats::residuals()], which means that, currently,
#' there is no recommended residual.
#'
#' @param object an object for which the extraction of model residuals is meaningful.
#'
#' @export
envel_resid <- function(object) {
  UseMethod("envel_resid")
}

#' @rdname envel_resid
#' @export
envel_resid.default <- function(object) abs(stats::residuals(object))

#' @rdname envel_resid
#' @export
envel_resid.glm <- function(object) {
  res_fun <- switch(object$family$family,
    binomial = ,
    poisson = stats::rstudent,
    stats::residuals.glm
  )
  abs(res_fun(object))
}

#' @rdname envel_resid
#' @export
envel_resid.lm <- function(object) abs(stats::rstudent(object))

#' Envelope Plot
#'
#' Plot AD_envelope
#'
#' Create envelope plot. The expected quantile, by default, is from a half-normal
#' distribution, as the default residuals from [envel_resid()] are absolute.
#' You may replace it with the `distribution` argument.
#'
#' @param x AD_envelope object, usually the result of [envelope()]
#' @param colors Vector of length 2, with color for points outside and inside
#'   the envelope band, respectively.
#' @param ylab The label for the y-axis.
#' @param xlab The label for the x-axis.
#' @param ylim the y limits of the plot.
#' @param distribution quantile function for reference theoretical distribution.
#' @param ... extra arguments passed to [graphics::plot]
#'
#' @export
plot.AD_envelope <- function(x,
                             colors = getOption("asympDiag.plot.AD_envelope.colors"),
                             xlab = "Expected quantiles",
                             ylab = "Observed quantiles",
                             distribution = function(p) stats::qnorm((1 + p) / 2),
                             ylim = base::range(c(x$observed, x$lower, x$upper)),
                             ...) {
  n <- length(x$observed)
  expected <- distribution(stats::ppoints(n))
  ord <- order(x$observed)
  graphics::plot(expected, x$observed[ord],
    col = ifelse(x$outside[ord], colors[1], colors[2]),
    type = "p", pch = 20, xlab = xlab, ylab = ylab,
    ylim = ylim,
    ...
  )
  graphics::lines(expected, x$lower[ord], lty = 1)
  graphics::lines(expected, x$upper[ord], lty = 1)
  graphics::lines(expected, x$med[ord], lty = 2)
}
