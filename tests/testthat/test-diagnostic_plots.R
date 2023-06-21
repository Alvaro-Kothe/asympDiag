test_that("Plot functions runs without problems", {
  fit <- simple_lm_fit()

  expect_no_error(plot_cook(fit))
  expect_no_error(plot_cook(fit, n_highlights = 9999999999))
  expect_no_error(plot_cook(fit, n_highlights = 2))
  expect_no_error(plot_cook(fit, cut = TRUE))
  expect_no_error(plot_res_vs_linear_predictor(fit))
  expect_no_error(plot_res_vs_linear_predictor(fit, pch = 20))
})
