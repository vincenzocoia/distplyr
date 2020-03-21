#' Mean of a Distribution
#'
#' Get the mean of a distribution.
#'
#' @param x A distribution object from which to obtain the mean.
#' @param ... Arguments to pass to the \code{integrate()} function (if needed).
#' @param verbose Print output of \code{integrate()} function?
#' @details If the mean is not already available in the distribution
#' object, and if the distribution is not a step distribution,
#' the mean is calculated using the \code{stats::integrate()}
#' function.
#' @return A single numeric.
#' @rdname meandst
#' @export
get_mean <- function(x, ...) UseMethod("get_mean")


#' @export
get_mean.dst <- function(x, ..., verbose = FALSE) {
	mu <- x$prop$mean
	if (!is.null(mu)) return(mu)
	cdf <- get_cdf(x)
	if (is.stepfun(cdf)) {
		y <- stats::knots(cdf)
		taus <- plateaus(cdf)
		probs <- diff(taus)
		return(sum(probs * y))
	}
	qf <- get_quantfn(x)
	int <- stats::integrate(qf, 0, 1)
	if (verbose) print(int)
	int$value
}

#' @export
get_mean.stepdst <- function(x, ...) {
	s <- steps(x)
	y <- s[["y"]]
	taus <- s[["tau"]]
	p <- diff(c(0, taus))
	stopifnot(sum(p) == 1)
	sum(p * y)
}
