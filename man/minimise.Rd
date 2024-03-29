% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verb-minimise.R
\name{minimise}
\alias{minimise}
\alias{minimize}
\title{Min Value of Several Distributions}
\usage{
minimise(..., draws = 1)

minimize(..., draws = 1)
}
\arguments{
\item{...}{Distribution objects, possibly also lists of distributions.}

\item{draws}{Number of draws from each distribution considered in the
minimum. Either a single numeric applying to all distributions in \code{...},
or a vector matching the number of distributions in \code{...}.}
}
\value{
A distribution of class \code{"min"}.
}
\description{
For a collection of distributions, this function provides the
distribution of the minimum value from independent draws of
each component distribution.
}
\details{
To use precise language, if \code{X1}, ..., \code{Xp} are
\code{p} independent random variables corresponding to the distributions
in \code{...}, then the distribution returned is of \code{min(X1, ..., Xp)}.
}
