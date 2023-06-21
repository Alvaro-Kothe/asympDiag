#' Perform Parametric Bootstrap Simulations
#'
#' This function performs parametric bootstrap simulations by generating new response values based on the fitted model,
#' refitting the model on these new responses, and computing a user-defined statistic for each simulated model.
#'
#' This function implements a parametric bootstrap procedure.
#' It generates new response values from the fitted model, refits
#' the model for each simulated response, and computes a user-defined statistic on the refitted model.
#' The refit function can be customized through the `refit_fn` argument.
#'
#' If `show_progress` is `TRUE`, the progress of the simulations will be
#' displayed using a progress bar from the `cli` package.
#'
#' @param model A fitted model object that will be used to simulate responses.
#' @param statistic A function that computes the desired statistic from the refitted model.
#'   It must take the refitted model as an argument.
#' @param nsim The number of simulations to perform.
#' @param responses An optional list of values to be used as response variables to refit the model.
#' @param refit_fn Function to refit the model with new responses. If `NULL`,
#'   defaults to `get_refit(model, y, ...)`.
#' @param show_progress Display a progress bar for the simulation iteration.
#' @param simplify logical or character string; should the result be simplified to a vector,
#'   matrix or higher dimensional array if possible?
#' @param stat_hc A function that verifies if the computed statistic is correct.
#'  It should return nothing, just throw errors to halt execution.
#' @param show_warning_count Show total of captured warnings from `refit_fn` as a warning.
#'  It only shows if the number of warnings is greater than 0.
#' @param show_not_converged_count Show total of models that didn't converge as a warning.
#'  It only shows if the number of models that didn't converged is greater than 0.
#' @param ... Additional arguments to be passed to `refit_fn`.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{`result`}{A list of length nsim if `simplify` is `FALSE`.
#'   Otherwise an atomic vector or matrix or list of length nsim.
#'  }
#'   \item{`responses`}{The list of simulated response values.}
#'   \item{`simulation_warning`}{A logical vector indicating whether a
#'     warning occurred during model refitting for each simulation.}
#'   \item{`converged`}{A logical vector indicating whether the model refit converged for each simulation.}
#' }
#'
#' @examples
#'
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' statistic <- function(model) coef(model)[["wt"]]
#' bootstrap_results <- parametric_bootstrap(model, statistic, nsim = 100)
#' wt_coefs <- Reduce(c, bootstrap_results$result)
#' summary(wt_coefs)
#'
#' @export
parametric_bootstrap <- function(model, statistic, nsim,
                                 responses = NULL, refit_fn = NULL, show_progress = TRUE,
                                 simplify = TRUE, stat_hc = NULL,
                                 show_warning_count = TRUE,
                                 show_not_converged_count = TRUE,
                                 ...) {
  result <- list()
  result$responses <- get_responses(responses, model, nsim)
  refit_fn <- default_refit_fn(refit_fn, model)
  nsim <- length(result$responses)
  result$result <- vector("list", nsim)
  result$simulation_warning <- logical(nsim)
  result$converged <- rep(NA, nsim)

  if (show_progress) cli::cli_progress_bar("Running simulation", total = nsim)
  for (i in seq_len(nsim)) {
    y_star <- result$responses[[i]]

    refit_result <- refit_safely(refit_fn, y_star, ...)
    result$simulation_warning[i] <- !is.null(refit_result$warning)
    model_refit <- refit_result$value
    if (show_progress) cli::cli_progress_update()

    if (is.null(model_refit)) {
      next
    }

    try(result$converged[i] <- get_converged(model_refit), silent = TRUE)
    stat <- statistic(model_refit)
    if (!is.null(stat_hc)) {
      stat_hc(stat)
    }
    result$result[[i]] <- stat
  }
  if (show_progress) cli::cli_progress_done()
  bootstrap_health_check(result$result, result$simulation_warning, result$converged,
    show_warning_count = show_warning_count,
    show_not_converged_count = show_not_converged_count
  )
  # from `sapply`
  if (!isFALSE(simplify)) {
    result$result <- simplify2array(result$result, higher = (simplify == "array"))
  }

  result
}

bootstrap_health_check <- function(result, warnings, converged,
                                   show_warning_count = TRUE,
                                   show_not_converged_count = TRUE) {
  if ((nf <- sum(sapply(result, is.null))) > 0) {
    if (nf == length(result)) {
      stop(
        "Could not refit any model. Verify the `refit_fn` and the `responses`."
      )
    }
    warning(nf, " statistics not computed.")
  }
  if (show_warning_count && (sw <- sum(warnings)) > 0) {
    warning(sw, " simulations threw warnings.")
  }
  if (show_not_converged_count && (nc <- sum(!converged, na.rm = TRUE)) > 0) {
    warning(nc, " simulations diverged.")
  }
}
