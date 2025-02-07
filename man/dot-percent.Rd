% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.percent}
\alias{.percent}
\title{Calculate Percent of Observations Within or Without a Window}
\usage{
.percent(x, window = NULL, within = TRUE)
}
\arguments{
\item{x}{A vector of values to evaluate. Required.}

\item{window}{An optional numeric vector giving a window.}

\item{within}{A logical value indicating whether to calculate the
percentage within the window (if \code{TRUE}) or the
percentage at or outside the window (if \code{FALSE}).
Defaults to \code{TRUE}.}
}
\value{
A list with the \code{Window}, if specified else \code{NULL},
the \code{Percent} of observations, and a \code{Label} specifying the
exact window used in human readable format.
}
\description{
This is an internal helper function to calculate and label
the percentage of a posterior distribution that falls within
the Region of Practical Equivalence (ROPE) or
at or beyond a Minimally Important Difference (MID).
It is designed to fail gracefully if no window given, and to
give some useful labels about the windows / range used.
Intended for use internally as part of \code{\link{brmsmargins}}.
}
\keyword{internal}
