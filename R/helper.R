report_refit_issues <- function(refit_result) {
  if (!is.null(refit_result$warning)) {
    message("Captured warning: ", refit_result$warning)
  }
  if (!is.null(refit_result$error)) {
    message("Captured error: ", refit_result$error)
  }
}

get_responses <- function(responses, model, nsim) {
  if (!is.null(responses)) {
    if (!is.list(responses)) {
      stop("`new_responses` should be a list")
    }
    responses
  } else {
    stats::simulate(model, nsim)
  }
}
