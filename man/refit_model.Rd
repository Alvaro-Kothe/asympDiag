% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_refit.R
\name{refit_model}
\alias{refit_model}
\title{Refit Model}
\usage{
refit_model(object, newresp, ...)
}
\arguments{
\item{object}{A model.}

\item{newresp}{the new response, may be a vector or a matrix.}

\item{...}{other arguments passed to \code{refit} or \code{update}.}
}
\value{
A model with same class as \code{object}.
}
\description{
Refit a model with a new response.
}
\details{
This function uses \code{newresp} to refit \code{object} replacing its old response variable.
If the class is \code{merMod} it uses \code{refit}, otherwise uses \code{\link[stats:update]{stats::update()}}.

The default method tries to update the model response using it's \code{\link[stats:model.frame]{stats::model.frame()}},
if it errors it tries to update the model by inserting the \code{newresp}
directly into the object formula.
}
