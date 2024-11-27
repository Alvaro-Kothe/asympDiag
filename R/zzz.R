.onLoad <- function(libname, pkgname) {
  op <- options()
  # nolint start: object_name_linter.
  op.asympDiag <- list(
    asympDiag.plot.AD_envelope.colors = c("red", "black")
  )
  # nolint end
  toset <- !(names(op.asympDiag) %in% names(op))
  if (any(toset)) options(op.asympDiag[toset])

  invisible()
}
