simple_x <- function() c(1, 3, 5, 7)

simple_y <- function() c(2, 3, 6, 9)

simple_lm_fit <- function() {
  x <- simple_x()
  y <- simple_y()
  lm(y ~ x)
}

lmer_data <- function() {
  data.frame(
    subject = rep(1:5, each = 3),
    time = rep(1:3, times = 5),
    y = c(5, 6, 7, 8, 9, 10, 4, 5, 6, 7, 8, 9, 6, 7, 8),
    group = rep(c("A", "B"), length.out = 15)
  )
}

simple_lmer_fit <- function() {
  data <- lmer_data()
  suppressWarnings(suppressMessages(lme4::lmer(y ~ time + group + (1 | subject), data = data)))
}

surv_data <- function() {
  data.frame(
    time = c(5, 10, 15, 20, 25, 30),
    status = c(1, 0, 1, 1, 0, 1),
    group = c("A", "A", "B", "B", "A", "B")
  )
}

simple_surv_fit <- function() {
  data <- surv_data()
  survival::survreg(survival::Surv(time, status) ~ group, data = data, dist = "exponential")
}
