#' Concatenate AD_pvalues object
#'
#' @param ld_pvalues list of elements of class `AD_pvalues`.
#'
#' @return Object of class `AD_pvalues`
#' @export
concat_pvalues <- function(ld_pvalues) {
  start <- ld_pvalues[[1]]
  rest <- ld_pvalues[-1]
  Reduce(concat_ld_pvalues, rest, init = start)
}

concat_ld_pvalues <- function(this, other) {
  if (!inherits(this, "AD_pvalues") || !inherits(other, "AD_pvalues")) {
    stop("Can concatenate only elements of class `AD_pvalues`")
  }
  if (!identical(this$test_coefficients, other$test_coefficients)) {
    stop("Test coefficients must be the same.")
  }
  this$pvalues_matrix <- cbind(this$pvalues_matrix, other$pvalues_matrix)
  this$pvalues_joint <- c(this$pvalues_joint, other$pvalues_joint)
  this$simulation_fixef <- append(this$simulation_fixef, other$simulation_fixef)
  this$simulation_vcov <- append(this$simulation_vcov, other$simulation_vcov)
  this$responses <- append(this$responses, other$responses)
  this$converged <- c(this$converged, other$converged)
  this$ginv_used <- c(this$ginv_used, other$ginv_used)
  this
}
