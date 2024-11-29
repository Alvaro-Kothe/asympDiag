#' Refit Model
#'
#' Refit a model with a new response.
#'
#' This function uses `newresp` to refit `object` replacing its old response variable.
#' If the class is `merMod` it uses `refit`, otherwise uses [stats::update()].
#'
#' The default method tries to update the model response using it's [stats::model.frame()],
#' if it errors it tries to update the model by inserting the `newresp`
#' directly into the object formula.
#'
#' @param object A model.
#' @param newresp the new response, may be a vector or a matrix.
#' @param ... other arguments passed to `refit` or `update`.
#' @return A model with same class as `object`.
#' @export
refit_model <- function(object, newresp, ...) {
  for (f in refitting_functions()) {
    res <- try(f(object, newresp, ...), silent = TRUE)
    if (!inherits(res, "try-error")) {
      return(res)
    }
  }
  stop("Refit failed.")
}

#' Find a working refitting function
#' Try to refit the model using the previously used response
#' @keywords internal
find_refit_fn <- function(object, y) {
  if (is.null(y)) {
    return(refit_model)
  }
  for (f in refitting_functions()) {
    res <- try(f(object, y), silent = TRUE)
    if (!inherits(res, "try-error")) {
      return(f)
    }
  }
  return(NULL)
}

#' Refit the Model
#'
#' Refit a regression model.
#'
#' This generic method refit the `object` with the newresp.
#' It shold maintain the `object` properties by default, but some aspects of
#' the default fitting method may be overwritten with the `...`.
#'
#' @inheritParams refit_model
#' @seealso [stats::update()], [lme4::refit()]
#' @keywords internal
get_refit <- function(object, newresp, ...) {
  # TODO: If there is a conventional generic method to refit a model,
  #  preferably from the `stats` package, use it and delete this method.
  UseMethod("get_refit")
}

default_refit_fn <- function(refit_fn, model) {
  if (!is.null(refit_fn)) {
    if (is.function(refit_fn)) {
      return(refit_fn)
    } else {
      stop("refit_fn should be a function that takes a vector as first argument.")
    }
  }

  y <- get_model_response(model)
  working_fit_fn <- find_refit_fn(model, y)
  if (is.null(working_fit_fn)) {
    stop(
      "Default refitting methods doesn't work.",
      " Define a refitting function in `refit_fn."
    )
  }
  function(new_y, ...) working_fit_fn(model, new_y, ...)
}

#' Calls f without throwing errors
#'
#' @param .f a function that refit a model.
#' @param newresp response variable used to fit the model.
#' @param ... arguments passed to other methods.
#' @keywords internal
refit_safely <- function(.f, newresp, ...) {
  warning_ <- NULL
  error_ <- NULL
  result <- tryCatch(
    withCallingHandlers(
      .f(newresp, ...),
      warning = function(w) {
        warning_ <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error_ <<- conditionMessage(e)
      NULL
    }
  )
  list(value = result, warning = warning_, error = error_)
}

refitting_functions <- function() {
  refitting_functions <- list(
    if (requireNamespace("lme4", quietly = TRUE)) lme4::refit else NULL, # for lme4::{,g}lmer and glmmTMB::glmmTMB
    get_refit,
    update_using_model_frame,
    update_using_formula
  )
}

#' Update the object with new response using only model frame.
#'
#' @inheritParams get_refit
update_using_model_frame <- function(object, newresp, ...) {
  if (as.character(stats::formula(object)[[2]])[[1]] == "c") {
    stop("Can't update a model with formula defined with `c`.")
  }
  dots <- list(...)
  if ("data" %in% names(dots)) {
    stop("The `data` argument should not be provided.")
  }
  model_frame <- stats::model.frame(object)
  if (is.vector(newresp)) {
    model_frame[, 1] <- newresp
  } else if (is.matrix(newresp)) {
    model_frame[[1]] <- NULL
    model_frame <- cbind(newresp, model_frame)
  }
  stats::update(object, data = model_frame, ...)
}

#' Update the object using only the formula
#'
#' Create a new formula with `newresp` values directly on the formula left hand side.
#'
#' This function is designed to be used as a fallback for [update_using_model_frame()]
#' as it's prone to run with memory issues.
#'
#' @inheritParams get_refit
update_using_formula <- function(object, newresp, ...) {
  dots <- list(...)
  if ("formula." %in% names(dots)) {
    stop("The `formula.` argument should not be provided.")
  }
  new_formula <- change_reponse_formula(newresp)
  stats::update(object, formula. = new_formula, ...)
}

collapse_vec <- function(x) {
  base::sprintf("c(%s)", paste0(x, collapse = ", "))
}

change_reponse_formula <- function(x) {
  formula_string <- if (is.matrix(x)) {
    columns_collapsed <- apply(x, 2, collapse_vec, simplify = FALSE)
    base::sprintf("cbind(%s) ~ .", paste(columns_collapsed, collapse = ", "))
  } else {
    base::sprintf("%s ~ .", collapse_vec(x))
  }
  stats::as.formula(formula_string)
}

# to mock
requireNamespace <- NULL # nolint
