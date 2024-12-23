% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/envelope.R
\name{envel_resid}
\alias{envel_resid}
\alias{envel_resid.default}
\alias{envel_resid.glm}
\alias{envel_resid.lm}
\title{Recommended Residual for Envelope}
\usage{
envel_resid(object)

\method{envel_resid}{default}(object)

\method{envel_resid}{glm}(object)

\method{envel_resid}{lm}(object)
}
\arguments{
\item{object}{an object for which the extraction of model residuals is meaningful.}
}
\description{
This function retrieves a function for recommended residuals for envelope.
\code{envel_resid()} has been superseded in favour of \code{\link[=envelope_residual]{envelope_residual()}}.
}
\details{
The residuals are absolute residuals to be plotted against the half-normal distribution.
For class \code{glm} the default is deviance, except for the families poisson and binomial,
where the default residual is \code{\link[=rstudent]{rstudent()}}, for deletion residuals.
For class \code{lm}, the default is also \code{\link[=rstudent]{rstudent()}}.
The default for other classes are \code{\link[stats:residuals]{stats::residuals()}}, which means that, currently,
there is no recommended residual.
}
