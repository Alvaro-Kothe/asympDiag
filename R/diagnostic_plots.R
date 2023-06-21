#' Plot Cook's distances
#'
#' @param model Model with [cooks.distance()] method
#' @param n_highlights The number of observations with the highest Cook's
#'   distance to highlight on the plot. Defaults to 0 (no highlights).
#' @param cut Logical. If TRUE, adds a cutoff line at the mean plus four times
#'   the standard deviation of Cook's distance. Defaults to FALSE.
#' @param xlab The label for the x-axis. Defaults to "Index".
#' @param ylab The label for the y-axis. Defaults to "Cook's distance".
#' @param ... Further arguments for [graphics::plot()]
#'
#' @return An invisible object representing Cook's distance values.
#'
#' @examples
#' fit <- lm(mpg ~ cyl, data = mtcars)
#'
#' plot_cook(fit)
#' plot_cook(fit, n_highlights = 2)
#'
#' @seealso \code{\link{cooks.distance}}, \code{\link{plot}}
#' @export
plot_cook <- function(model, n_highlights = 0, cut = FALSE,
                      xlab = "Index", ylab = "Cook's distance", ...) {
  cook <- stats::cooks.distance(model)
  ylim <- grDevices::extendrange(cook)

  plot(seq_along(cook), cook,
    pch = 20, xlab = xlab, ylab = ylab, ylim = ylim,
    ...
  )
  if (n_highlights > 0) {
    highests <- order(cook, decreasing = TRUE)[seq_len(min(c(n_highlights, length(cook))))]
    graphics::text(
      x = highests, y = cook[highests], label = highests,
      pos = 3
    )
  }
  if (cut) {
    graphics::abline(h = base::mean(cook) + 4 * stats::sd(cook))
  }

  return(invisible(cook))
}

#' Plot Residuals against Linear Predictor
#'
#' @param model Model with methods [predict()] or [fitted()]
#' @param residual_fn A function to calculate model residuals. The default is
#'   `stats::rstandard`.
#' @param xlab The label for the x-axis. Defaults to "Linear Predictor".
#' @param ylab The label for the y-axis. Defaults to "Standardized deviance residuals".
#' @param ... Extra arguments to `residual_fn` and [plot()].
#'
#' @return An invisible list containing the linear predictor (x) and standardized
#'   deviance residuals (y).
#'
#' @details
#' If the model was fitted using the [glm()] function, it will use the [predict()]
#' method with `type = link`, otherwise, it will use the [fitted()] method.
#'
#' @examples
#' fit <- lm(mpg ~ cyl, data = mtcars)
#'
#' plot_res_vs_linear_predictor(fit)
#' plot_res_vs_linear_predictor(fit, residual_fn = rstudent)
#' plot_res_vs_linear_predictor(fit, residual_fn = residuals)
#'
#'
#' glm_fit <- glm(cyl ~ mpg, family = poisson(), data = mtcars)
#'
#' plot_res_vs_linear_predictor(glm_fit)
#' plot_res_vs_linear_predictor(glm_fit, type = "pearson")
#'
#' @export
plot_res_vs_linear_predictor <- function(model, residual_fn = stats::rstandard,
                                         xlab = "Linear Predictor",
                                         ylab = "Standardized deviance residuals",
                                         ...) {
  linear_predictor <- switch(class(model)[1],
    glm = stats::predict(model, type = "link"),
    stats::fitted(model)
  )
  y <- residual_fn(model, ...)
  plot(x = linear_predictor, y, xlab = xlab, ylab = ylab, ...)

  return(invisible(list(x = linear_predictor, y = y)))
}
