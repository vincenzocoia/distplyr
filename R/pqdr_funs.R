#' Calculate cdf
#'
#' @param object Object of class "dst"
#' @param q Vector of values to evaluate the cdf at
#' @return Vector of cdf values if \code{q} is present; otherwise,
#' returns a function for the cdf.
#' @seealso \code{\link{qdst}}, \code{\link{ddst}}, \code{\link{rdst}}
#' @export
pdst <- function(object, q) UseMethod("pdst")

#' @export
pdst.dst <- function(object, q) {
	cdf <- object$pdst
	if (missing(q)) return(cdf)
	cdf(q)
}

#' Calculate quantile function
#'
#' @param object Object of class "dst"
#' @param p Vector of values to evaluate the quantile function at
#' @return Vector of quantiles if \code{p} is present; otherwise,
#' returns a function for the quantile function.
#' @seealso \code{\link{pdst}}, \code{\link{ddst}}, \code{\link{rdst}}
#' @export
qdst <- function(object, p) UseMethod("qdst")

#' @export
qdst.dst <- function(object, p) {
	qf <- object$qdst
	if (missing(p)) return(qf)
	qf(p)
}

#' Calculate density/mass function
#'
#' @param object Object of class "dst"
#' @param x Vector of values to evaluate the density or mass function at
#' @return Vector of density/mass values if \code{x} is present; otherwise,
#' returns a function for the pdf/pmf.
#' @seealso \code{\link{qdst}}, \code{\link{pdst}}, \code{\link{rdst}}
#' @export
ddst <- function(object, x) UseMethod("ddst")

#' @export
ddst.dst <- function(object, x) {
	f <- object$ddst
	if (missing(x)) return(f)
	f(x)
}

#' Generate random sample from a distribution
#'
#' @param object Object of class "dst"
#' @param n Number of observations to generate
#' @return Vector of independent values drawn from the inputted distribution
#' if \code{n} is present; otherwise,
#' returns a function for a random number generator.
#' @seealso \code{\link{qdst}}, \code{\link{ddst}}, \code{\link{pdst}}
#' @export
rdst <- function(object, n) UseMethod("rdst")

#' @export
rdst.dst <- function(object, n) {
	r <- object$rdst
	if (missing(n)) return(r)
	r(n)
}
