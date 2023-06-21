simple_lm_fit <- function() {
  x <- c(1, 3, 5, 7)
  y <- c(2, 3, 6, 9)
  lm(y ~ x)
}
