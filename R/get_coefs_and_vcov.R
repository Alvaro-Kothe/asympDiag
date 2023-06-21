#' Get fixed effects
#'
#' Extracts the fixed effects coefficients from a model.
#'
#' By default it calls [stats::coef()]. If the model class is `merMod` calls
#' `fixef`.
#'
#' @param object A model object for which fixed effects are to be retrieved.
#' @return A numeric vector of fixed effects coefficients.
#'
#' @export
get_fixef <- function(object) {
  UseMethod("get_fixef")
}

#' @rdname get_fixef
#' @export
get_fixef.default <- function(object) {
  stats::coef(object)
}

#' @rdname get_fixef
#' @export
get_fixef.merMod <- function(object) {
  lme4::fixef(object)
}

#' @rdname get_fixef
#' @export
get_fixef.glmmTMB <- function(object) {
  glmmTMB::fixef(object)$cond
}

#' Get covariance matrix
#'
#' Retrieves the covariance matrix for the fixed effects.
#'
#' By default it calls [stats::vcov()] to retrieve the covariances.
#'
#' @param object A model object for which the covariance matrix is to be retrieved.
#' @return A covariance matrix of the model object.
#' @export
get_vcov <- function(object) {
  UseMethod("get_vcov")
}

#' @rdname get_vcov
#' @export
get_vcov.default <- function(object) {
  as.matrix(stats::vcov(object))
}

#' @rdname get_vcov
#' @export
get_vcov.glmmTMB <- function(object) {
  as.matrix(stats::vcov(object)$cond)
}

#' Did the model converged
#'
#' Retrieves if the model converged.
#'
#' The default method retrieves `object$converged`.
#' For models of class merMod it verifies if the infinity norm of the
#' Newton–Raphson step is less than 0.0001.
#'
#' @param object A model to check for convergence.
#' @return A boolean value.
#' @export
get_converged <- function(object) {
  UseMethod("get_converged")
}

#' @rdname get_converged
#' @export
get_converged.default <- function(object) {
  object$converged
}
