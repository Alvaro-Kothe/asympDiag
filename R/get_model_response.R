#' Extract the Response Variable from a Model Object
#'
#' This function is designed to extract the response variable from a fitted model object.
#'
#' The default method attempts to create a model frame using [model.frame()].
#' Any error encountered during this process is caught and
#' results in a `NULL` return value.
#'
#' @param object A fitted model from which the response variable should be extracted.
#'
#' @return The response variable extracted from the model. If it couldn't be extracted,
#'  the function returns `NULL`.
#'
#' @examples
#' lm_model <- lm(mpg ~ wt + cyl, data = mtcars)
#' response <- get_model_response(lm_model)
#' print(response)
#'
#' @export
get_model_response <- function(object) {
  UseMethod("get_model_response")
}

#' @export
get_model_response.default <- function(object) {
  mf <- tryCatch(
    stats::model.frame(object),
    error = function(e) NULL
  )
  if (is.null(mf)) {
    return(NULL)
  }
  mf[[1]]
}
