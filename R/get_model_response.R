#' Get Model Response
#' @param object Model to extract responses from.
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
