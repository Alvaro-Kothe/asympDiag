.onLoad <- function(libname, pkgname) {
  op <- options()
  op.asympDiag <- list(
    asympDiag.plot.AD_envelope.colors = c("red", "black")
  )
  toset <- !(names(op.asympDiag) %in% names(op))
  if (any(toset)) options(op.asympDiag[toset])

  invisible()
}
