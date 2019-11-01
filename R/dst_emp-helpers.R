#' Empirical Functions for Distributions
#'
#' Makes empirical mass and quantile functions from a univariate
#' sample, just as \code{stats::ecdf()} makes empirical cdf's.
#'
#' @param x Numeric vector of the observations for which to make the quantile
#' function
#' @return Vectorized functions; quantile function for \code{eqf()}, and
#' probability mass function for \code{epmf()}.
#' @seealso \code{\link{dst_emp}}
#' @rdname efun
#' @export
eqf <- function(x) {
	function(p) {
		quants <- unname(stats::quantile(x, probs = p, type = 1, na.rm = TRUE))
		ifelse(p == 0, -Inf, quants)
	}
}

#' @rdname efun
#' @export
epmf <- function(x) {
	vals <- unique(sort(x))
	counts <- unname(unclass(table(x)))
	n <- sum(counts)
	p <- counts / n
	Vectorize(function(y) mean(y == x, na.rm = TRUE))
}



