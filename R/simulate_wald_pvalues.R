#' Generate Wald test P-Values with Monte Carlo Simulations
#'
#' This function performs Monte Carlo simulations to generate p-values for
#' model coefficients by refitting the model with new simulated responses and
#' computing the Wald test statistics for each simulation.
#' It's standard behavior verify if the type I error from Wald tests are under control, considering the provided model
#' as "true", i.e., the model assumptions are valid.
#' It supports univariate and joint Wald tests, using chi-squared
#' distributions to calculate p-values.
#'
#' If `responses` is provided, the function refits the model with these new
#' response vectors. Otherwise, it generates new responses with [stats::simulate()].
#'
#' For each new response calls [get_refit()] to generate a new model with the new
#' response. It gets the fixed effects and the variance and covariance matrix with
#' [get_fixef()] and [get_vcov()].
#'
#' Each simulated model is refitted using the specified `refit_fn`
#' (or the default refit function) and the fixed effects coefficients and
#' variance-covariance matrix are extracted using `coef_fn` and `vcov_fn`, respectively.
#' The univariate Wald test is computed from the Wald statistic for each
#' coefficient, while the joint Wald test uses the inverse variance-covariance
#' matrix to compute a Wald statistic for the test_coefficients.
#' P-values are calculated from a chi-squared distribution with appropriate degrees of freedom.
#'
#' @param test_coefficients Numeric vector. A vector with values to be used to compute
#'   the test statistic. It should be the coefficients that was used to compute
#'   the fitted values of the response. If `NULL` defaults to `coef_fn(model)`.
#' @param coef_fn Function that retrieves the coefficients of the model.
#' @param vcov_fn Function that computes the variance-covariance matrix for the models
#'   adjusted in the simulations.
#' @param plot.it Logical. Generate ecdf plot for joint Wald test.
#' @inheritParams parametric_bootstrap
#'
#' @return An object of class `AD_pvalues`, which contains the following components:
#' \describe{
#'   \item{test_coefficients}{Vector of coefficients being tested.}
#'   \item{pvalues_matrix}{Matrix of p-values where each column corresponds to a
#'          simulation and each row corresponds to a coefficient.}
#'   \item{pvalues_joint}{Vector containing the joint p-values obtained from each simulation.}
#'   \item{simulation_fixef}{List of fixed effect coefficient estimates from each simulation.}
#'   \item{simulation_vcov}{List of covariance matrices estimated from each simulation.}
#'   \item{simulation_warning}{Vector of boolean indicating if a simulation threw a warning.}
#'   \item{converged}{Logical vector indicating whether model refitting converged for each simulation.}
#'   \item{responses}{Simulated responses used for refitting the model.}
#' }
#' @seealso [plot.AD_pvalues()] for plotting.
#' @examples
#' # from help("glm")
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' model <- glm(counts ~ outcome + treatment, family = poisson())
#' new_responses <- replicate(100, MASS::rnegbin(fitted.values(model), theta = 4.5), simplify = FALSE)
#' simulate_wald_pvalues(model, responses = new_responses, nsim = 100)
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
#'   simulate_wald_pvalues(fit, responses = new_responses, refit_fn = refit_surv_ovarian)
#' }
#' @export
simulate_wald_pvalues <- function(model, nsim = 1000, responses = NULL,
                                  test_coefficients = NULL,
                                  refit_fn = NULL, coef_fn = get_fixef, vcov_fn = get_vcov,
                                  show_progress = TRUE, plot.it = TRUE,
                                  ...) {
  if (is.null(test_coefficients)) {
    test_coefficients <- coef_fn(model)
  }
  if (is.null(names(test_coefficients))) {
    names(test_coefficients) <- paste0("par_", seq_along(test_coefficients))
  }

  refit_fn <- default_refit_fn(refit_fn, model)

  responses <- get_responses(responses, model, nsim)

  sim_result <- parametric_bootstrap(
    model = model, statistic = function(x) list(coef_fn(x), vcov_fn(x)),
    nsim = nsim, refit_fn = refit_fn, responses = responses,
    simplify = FALSE, show_progress = show_progress,
    stat_hc = NULL, ...
  )
  pvalues <- compute_pvalues(sim_result$result, test_coefficients)
  p_values_health_check(pvalues)

  out <- list(
    simulation_fixef = lapply(sim_result$result, function(x) x[[1]]),
    simulation_vcov = lapply(sim_result$result, function(x) x[[2]]),
    simulation_warning = sim_result$simulation_warning,
    converged = sim_result$converged,
    responses = responses, pvalues_matrix = pvalues$matrix, pvalues_joint = pvalues$joint,
    test_coefficients = test_coefficients
  )

  class(out) <- "AD_pvalues"
  if (plot.it) {
    ncoef <- length(test_coefficients)
    plot(out, which = ncoef + 1)
    return(invisible(out))
  }
  out
}

p_values_health_check <- function(x) {
  if ((nsing <- sum(is.na(x$joint))) > 0) {
    warning("Couldn't inverse vcov from ", nsing, " simulations.")
  }
}

compute_pvalues <- function(result, test_coefficients) {
  nsim <- length(result)
  ncoef <- length(test_coefficients)
  joint <- rep(NA_real_, nsim)
  mat <- matrix(NA_real_, ncoef, nsim, dimnames = list(names(test_coefficients)))
  for (i in seq_len(nsim)) {
    if (is.null(result[[i]])) next
    coef_ <- result[[i]][[1]]
    vc <- result[[i]][[2]]
    wald_stat_uni <- (coef_ - test_coefficients)^2 / diag(vc)
    mat[, i] <- 1 - stats::pchisq(wald_stat_uni, 1)

    joint[[i]] <- compute_p_values_joint(coef_, vc, test_coefficients)
  }
  list(matrix = mat, joint = joint)
}

compute_p_values_joint <- function(coefs, vcov, generator_coef) {
  vcov_inv <- tryCatch(solve(vcov),
    error = function(e) {
      NULL
    }
  )
  if (is.null(vcov_inv)) {
    return(NA_real_)
  }
  dif_nul <- coefs - generator_coef
  chisq_stat <- t(dif_nul) %*% vcov_inv %*% dif_nul

  1 - stats::pchisq(chisq_stat, length(generator_coef))
}

#' Plot Empirical Cumulative Distribution Function (ECDF) of p-values
#'
#' This function creates several plots with the empirical cumulative distribution
#' of the p-values obtained through simulation.
#'
#' If the asymptotic approximation is valid the distribution of the p-values
#' should be close to an uniform distribution.
#' Discrepancies are highlighted, by default it verifies the significance on the
#' most commonly used significance values are 0.01, 0.05 and 0.10.
#'
#' The reported KS (Kolmogorov-Smirnov) test is the result of the "two-sided" [stats::ks.test()] function
#' comparing the observed p-values distribution with the uniform.
#' The test may reject the KS test due to few simulations, make sure that the lines
#' shown in the plot are smooth before drawing any conclusions.
#'
#' @param x AD_pvalues object, usually the result of [simulate_wald_pvalues()]
#' @param which A vector specifying the indices of coefficients to plot.
#'  If index is bigger than the number of coefficients it plots the joint p_value.
#' @param caption A character vector or a list with caption for each plot.
#'  If it's a list, the list index must match the coefficient index used by `which`.
#'  If it's a vector, it's values are used in order.
#' @param ks_test If `TRUE` inserts Kolmogorov-Smirnov p-value in the graphic.
#' @param signif Points to verify discrepancy.
#' @param discrepancy_tol Threshold to consider point discrepant.
#' @param plot_uniform Logical. If TRUE, plot uniform distribution.
#' @param uniform_legend Logical. If TRUE, a legend is added to the plot to
#'   distinguish between the p-value and U(0, 1) curves. Defaults to TRUE.
#' @param converged_only Use p-values from converged models only.
#' @param no_warnings If TRUE, ignore simulations that threw warnings.
#' @param ylab The label for the y-axis. Defaults to "Empirical cumulative distribution".
#' @param xlab The label for the x-axis. Defaults to "p-value".
#' @param ... extra arguments passed to [graphics::plot]
#' @param ask Logical. If TRUE, the user is prompted before each plot. Defaults
#'   to TRUE if in an interactive session and the number of plots is greater
#'   than the available space; otherwise, FALSE.
#'
#' @return A vector of joint p-values for all coefficients.
#'
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' p_values_ld <- simulate_wald_pvalues(model, n_sim = 100)
#' plot(p_values_ld)
#' @export
plot.AD_pvalues <- function(x,
                            which = seq_len(length(x$test_coefficients) + 1),
                            caption = as.list(paste("ECDF of", c(names(x$test_coefficients), "all coefficients"))),
                            ks_test = TRUE, signif = c(0.01, 0.05, 0.10),
                            discrepancy_tol = .1,
                            plot_uniform = TRUE, uniform_legend = TRUE,
                            converged_only = FALSE, no_warnings = FALSE,
                            ylab = "Empirical cumulative distribution", xlab = "p-value",
                            ...,
                            ask = prod(graphics::par("mfcol")) < length(which) && grDevices::dev.interactive()) {
  if (ask) {
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask))
  }
  mask <- if (converged_only) x$converged else TRUE
  if (no_warnings) {
    mask <- mask & (!x$simulation_warning)
  }
  for (plot_index in seq_along(which)) {
    i <- which[[plot_index]]
    title <- if (is.list(caption)) caption[[i]] else caption[plot_index]
    plot_joint <- i > length(x$test_coefficients)
    p_values <- if (plot_joint) x$pvalues_joint else x$pvalues_matrix[i, ]
    p_values <- p_values[mask & !is.na(p_values)]
    if (length(p_values) == 0L) {
      stop("No p-value to plot.")
    }

    plot_ecdf_pvalue(p_values,
      ks_test = ks_test, signif = signif,
      discrepancy_tol = discrepancy_tol,
      plot_uniform = plot_uniform, uniform_legend = uniform_legend,
      main = title, xlab = xlab, ylab = ylab, ...
    )
  }
}

#' Plot Empirical Cumulative Distribution Function (ECDF) of p-values
#'
#' @inheritParams plot.AD_pvalues
#' @param p_values vector of p-values
#' @param main main caption passed to [plot]
#' @export
plot_ecdf_pvalue <- function(p_values,
                             ks_test = TRUE, signif = c(0.01, 0.05, 0.10),
                             discrepancy_tol = 0.10,
                             plot_uniform = TRUE, uniform_legend = TRUE,
                             main = "",
                             ylab = "Empirical cumulative distribution", xlab = "p-value",
                             ...) {
  ecdf_ <- stats::ecdf(p_values)
  alpha_ <- seq(-0.01, 1.01, length.out = 201)
  plot(
    alpha_, ecdf_(alpha_),
    type = "l",
    main = main, ylab = ylab, xlab = xlab, ...
  )
  if (ks_test) {
    ks_res <- test_uniform_dist(p_values)$p.value
    graphics::legend("top", legend = sprintf("KS p-value: %.5f", ks_res), bty = "n")
  }
  discrepancies <- character()
  for (expected_rejection in signif) {
    observed <- ecdf_(expected_rejection)
    discrepancy <- (observed / expected_rejection) - 1.0
    if (abs(discrepancy) > discrepancy_tol) {
      discrepancies <- c(discrepancies, sprintf(
        "%.2f: %.3f (%+.0f%%)",
        expected_rejection, observed, discrepancy * 100
      ))
      graphics::segments(expected_rejection, 0.0, y1 = observed, lty = 2)
    }
  }
  if (length(discrepancies) > 0) {
    graphics::legend("bottomright", discrepancies, bty = "n")
  }
  if (plot_uniform) {
    graphics::lines(alpha_, stats::punif(alpha_), lty = 2, col = "gray30")
    if (uniform_legend) {
      graphics::legend("topleft",
        legend = c("p-value", "U(0, 1)"),
        lty = c(1, 2),
        col = c("black", "gray30")
      )
    }
  }
}


test_uniform_dist <- function(x) {
  suppressWarnings(stats::ks.test(x, stats::punif))
}
