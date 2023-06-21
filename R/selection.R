remove_variable <- function(x) {
  intercept <- "(Intercept)"
  remove_intercept <- intercept %in% x

  rhs <- paste0(x, collapse = "-")
  if (remove_intercept) rhs <- paste(rhs, "+ 0")
  out <- stats::as.formula(paste0(". ~ . - ", rhs))

  return(out)
}

add_variable <- function(x) {
  rhs <- paste0(x, collapse = "+")
  out <- stats::as.formula(paste0(". ~ . + ", rhs))

  return(out)
}

coefs_hash <- function(names_coefs) {
  paste(sort(names_coefs), collapse = ",")
}

backward_values <- function(model,
                            measure_fn,
                            measure_one_at_time,
                            do_not_remove,
                            seen_states,
                            hashtable = NULL,
                            ...) {
  nargs_measure_fn <- length(formals(measure_fn))
  cur_coefs <- names(get_fixef(model))
  candidates_remove <- stats::setNames(nm = setdiff(cur_coefs, do_not_remove))

  evaluate_one_at_time <- measure_one_at_time || nargs_measure_fn == 2L

  possible_next_states <- lapply(candidates_remove, function(x) {
    names(get_fixef(model))[names(get_fixef(model)) != x]
  })

  next_state_seen <- sapply(
    possible_next_states,
    function(x) any(sapply(seen_states, setequal, x))
  )

  removed_vars <- names(next_state_seen)[!next_state_seen]

  if (!evaluate_one_at_time) {
    return(measure_fn(model)[removed_vars])
  }

  values <- double(sum(!next_state_seen))
  names(values) <- removed_vars
  eval_model <- function(cur_model) {
    if (nargs_measure_fn == 1L) {
      measure_fn(cur_model)
    } else if (nargs_measure_fn == 2L) {
      measure_fn(model, cur_model)
    } else {
      stop("`measure_fn` should have 1 or 2 arguments")
    }
  }

  possible_next_states <- possible_next_states[!next_state_seen]
  for (i in seq_along(possible_next_states)) {
    key <- coefs_hash(possible_next_states[[i]])
    prev_fit_model <- hashtable[[key]]
    if (!is.null(prev_fit_model)) { # Using the fact the indexing NULL returns NULL
      values[i] <- eval_model(prev_fit_model)
    } else {
      removed_var <- removed_vars[i]

      next_formula <- remove_variable(removed_var)

      next_fit <- stats::update(model, formula. = next_formula, ...)
      values[i] <- eval_model(next_fit)
      if (!is.null(hashtable)) hashtable[[key]] <- next_fit
    }
  }
  return(values)
}

forward_values <- function(model,
                           measure_fn,
                           measure_one_at_time,
                           addable_coefs,
                           seen_states,
                           hashtable = NULL,
                           ...) {
  nargs_measure_fn <- length(formals(measure_fn))
  add_candidates <- stats::setNames(nm = setdiff(addable_coefs, names(get_fixef(model))))
  possible_next_states <- lapply(add_candidates, c, names(get_fixef(model)))
  next_state_seen <- sapply(
    possible_next_states,
    function(x) any(sapply(seen_states, setequal, x))
  )
  add_candidates <- add_candidates[!next_state_seen]
  evaluate_one_at_time <- measure_one_at_time || nargs_measure_fn == 2L

  eval_model <- function(cur_model, var_name = NULL) {
    if (!evaluate_one_at_time) {
      return(measure_fn(cur_model)[[var_name]])
    } else {
      if (nargs_measure_fn == 2L) {
        return(measure_fn(cur_model, model))
      } else {
        return(measure_fn(cur_model))
      }
    }
  }

  possible_next_states <- possible_next_states[add_candidates]
  values <- double(length(add_candidates))
  names(values) <- add_candidates

  for (i in seq_along(possible_next_states)) {
    key <- coefs_hash(possible_next_states[[i]])
    prev_fit_model <- hashtable[[key]]
    added_var <- add_candidates[i]
    if (!is.null(prev_fit_model)) { # Using the fact the indexing NULL returns NULL
      values[i] <- eval_model(prev_fit_model, added_var) # nocov
    } else {
      next_formula <- add_variable(added_var)
      next_fit <- stats::update(model, formula. = next_formula, ...)
      values[i] <- eval_model(next_fit, added_var)
      if (!is.null(hashtable)) hashtable[[key]] <- next_fit
    }
  }

  values
}

update_model_remove <- function(model,
                                values,
                                threshold,
                                minimize_only = FALSE,
                                hashtable = NULL,
                                ...) {
  to_remove <- if (minimize_only) which.min(values) else which.max(values)
  if (length(to_remove) == 0L) {
    return(NULL)
  }

  value <- values[to_remove]
  cant_remove <- if (!minimize_only) value <= threshold else value >= threshold
  if (cant_remove) {
    return(NULL)
  }
  removed_var <- names(to_remove)
  remaining_coefs <- names(get_fixef(model))[
    names(get_fixef(model)) != removed_var
  ]
  key <- coefs_hash(remaining_coefs)
  prev_fit_model <- hashtable[[key]]
  if (!is.null(prev_fit_model)) {
    return(list(fit = prev_fit_model, removed_var = value))
  }
  next_formula <- remove_variable(removed_var)

  next_fit <- stats::update(model, formula. = next_formula, ...)

  if (!is.null(hashtable)) hashtable[[key]] <- next_fit
  return(list(fit = next_fit, removed_var = value))
}

update_model_add <- function(model, values, threshold, hashtable = NULL, ...) {
  to_add <- which.min(values)
  if (length(to_add) == 0L) {
    return(NULL)
  }

  value <- values[to_add]

  if (value > threshold) {
    return(NULL)
  }

  added_var <- names(to_add)
  new_coefs <- c(names(get_fixef(model)), added_var)
  key <- coefs_hash(new_coefs)
  prev_fit_model <- hashtable[[key]]
  if (!is.null(prev_fit_model)) {
    return(list(fit = prev_fit_model, added_var = value))
  }
  next_formula <- add_variable(added_var)

  next_fit <- stats::update(model, formula. = next_formula, ...)
  if (!is.null(hashtable)) hashtable[[key]] <- next_fit # nocov

  return(list(fit = next_fit, added_var = value))
}


#' Select covariates
#'
#' @param model A model with [stats::update()], [stats::coef()] methods.
#' @param threshold Value threshold to remove variable. It can be a fixed value
#' or a function. The variable is removed if `measure_fn(model) > threshold` and
#' added if `measure_fn(model) <= threshold`.
#' @param direction The direction of variable selection. Options include "backward",
#'   "forward", or "both". Defaults to "both".
#' @param addable_coefs A vector of coefficients that can be added during forward selection.
#'   Defaults to all coefficients in the model.
#' @param measure_fn Function with model as argument and returns values to be used by
#' `threshold`. It can also compare two models, where during forward step
#' it calls `measure_fn(candidate_model, current_selected_model)` and
#' during backward step it calls `measure_fn(current_selected_model, candidate_model)`.
#' Defaults to the p-value from the summary of the coefficients.
#' @param measure_one_at_time Boolean indicating to apply `measure_fn` to each
#' variable individually during forward and backward steps.
#' Set this option to `TRUE` if `measure_fn` returns an atomic value, for example if
#' `measure_fn` is `AIC`.
#' @param minimize_only Logical indicating that during backward model update
#' it should minimize the `measure_fn` instead of maximize it.
#' @param max_steps The maximum number of steps for the variable selection process.
#'   Defaults to 1000.
#' @param return_step_results Logical. If TRUE, the function returns a list
#'   containing the final fitted model and a log of the selection steps.
#'   Defaults to FALSE.
#' @param do_not_remove A character vector specifying variables that should not
#'   be removed during backward selection. Defaults to "(Intercept)".
#' @param ... Extra arguments to [stats::update()].
#'
#' @return A fitted model with selected covariates based on the variable selection process.
#'   If \code{return_step_results} is TRUE, a list containing the final fitted model
#'   and a log of the selection steps is returned.
#'
#' @examples
#' model <- lm(mpg ~ ., data = mtcars)
#' select_covariates(model)
#'
#' ## measure_fn with two parameters
#'
#' lrt <- function(model1, model2) {
#'   lrt_stat <- 2 * (logLik(model1)[1L] - logLik(model2)[1L])
#'   return(1 - pchisq(lrt_stat, 1))
#' }
#'
#' select_covariates(model, measure_fn = lrt)
#'
#' ## AICc selection
#'
#' AICc <- function(model) {
#'   loglike <- logLik(model)
#'   df <- attr(loglike, "df")
#'   nobs <- attr(loglike, "nobs")
#'   aic <- -2 * as.numeric(loglike) + 2 * df
#'
#'   aicc <- aic + (2 * (df^2) + 2 * df) / (nobs - df - 1)
#'
#'   return(aicc)
#' }
#'
#' selection <- select_covariates(model,
#'   measure_fn = AICc,
#'   threshold = AICc,
#'   measure_one_at_time = TRUE,
#'   minimize_only = TRUE,
#'   direction = "both",
#'   data = mtcars
#' )
#'
#' @export
select_covariates <- function(model,
                              threshold = .15,
                              direction = c("both", "backward", "forward"),
                              addable_coefs = names(get_fixef(model)),
                              measure_fn = function(x) summary(x)[["coefficients"]][, 4],
                              measure_one_at_time = FALSE,
                              minimize_only = FALSE,
                              max_steps = 1000,
                              return_step_results = FALSE,
                              do_not_remove = c("(Intercept)"), ...) {
  # nocov start
  if (utils::packageVersion("utils") < "4.2.0") {
    models <- NULL
  } else {
    models <- utils::hashtab()
    models[[coefs_hash(names(get_fixef(model)))]] <- model
  } # nocov end
  direction <- match.arg(direction)
  log_ <- list()
  cur_step <- 0
  seen_states <- list(names(get_fixef(model)))
  do_backward <- direction %in% c("backward", "both")
  do_forward <- direction %in% c("forward", "both")

  threshold_fn <- if (is.function(threshold)) {
    threshold
  } else {
    function(model_) threshold
  }

  while (cur_step < max_steps) {
    cur_step <- cur_step + 1

    cur_threshold <- threshold_fn(model)

    # 1. Backward removal
    if (do_backward && length(get_fixef(model)) > 1) {
      values <- backward_values(
        model = model,
        measure_fn = measure_fn,
        measure_one_at_time = measure_one_at_time,
        do_not_remove = do_not_remove,
        seen_states = seen_states,
        hashtable = models,
        ...
      )

      updated_model <- update_model_remove(
        model = model,
        values = values,
        threshold = cur_threshold,
        minimize_only = minimize_only,
        hashtable = models,
        ...
      )

      if (!is.null(updated_model)) {
        model <- updated_model$fit
        seen_states <- append(seen_states, list(names(get_fixef(model))))
        log_[[cur_step]] <- list(
          step = cur_step, operation = "removal",
          value = updated_model$removed_var
        )
        next
      }
    }

    # 2. Forward selection
    if (do_forward) {
      values <- forward_values(
        model = model,
        measure_fn = measure_fn,
        measure_one_at_time = measure_one_at_time,
        addable_coefs = addable_coefs,
        seen_states = seen_states,
        hashtable = models,
        ...
      )

      updated_model <- update_model_add(
        model,
        values,
        cur_threshold,
        hashtable = models,
        ...
      )

      if (!is.null(updated_model)) {
        model <- updated_model$fit
        seen_states <- append(seen_states, list(names(get_fixef(model))))
        log_[[cur_step]] <- list(
          step = cur_step, operation = "addition",
          value = updated_model$added_var
        )
        next
      }
    }

    # If cant remove or add a variable stop the selection
    break
  }

  if (return_step_results) {
    return(list(fit = model, log = log_))
  }

  return(model)
}
