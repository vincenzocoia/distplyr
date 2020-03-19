#' Create a Step Distribution
#'
#' A step distribution is one where the cdf
#' and quantile function are step functions.
#' \code{stepdst()} facilitates the creation of
#' such a distribution by specifying the
#' pairs associated with the closed points
#' at each breakpoint.
#'
#' @param formula An object of class "formula",
#' of the form tau (probabilities) ~ y (quantiles).
#' @param data Data frame, list, or environment
#' containing the variables in the formula. If missing,
#' expecting the data to be contained in the formula.
#' @param ... Additional arguments to be passed to \code{\link{dst}}.
#' @return A "stepdst" object, which is also a "dst" object,
#' containing cdf and quantile functions. If you'd like to add other
#' functions through the \code{\link{dst}} function, you can do so
#' via \code{...}.
#' @note When creating your pairs,
#' keep in mind that a cdf is right-continuous,
#' and a quantile function is left-continuous.
#' If specifying points from a cdf, then, take tau
#' to be the upper level of a breakpoint; if specifying
#' from a quantile function, take y to be the lower level
#' of a breakpoint.
#'
#' This also means that your probabilities should contain
#' 1, but need not contain 0.
#' @export
stepdst <- function(formula, data, ...) {
	this_call <- match.call()
	id <- match(c("formula", "data"), names(this_call), nomatch = 0)
	mf_call <- this_call[c(1L, id)]
	mf_call[[1L]] <- quote(stats::model.frame)
	mf <- eval.parent(mf_call)
	mf <- mf[order(mf[[1]]), ]
	taus <- mf[[1]]
	y    <- mf[[2]]
	if (!is.numeric(taus) || any(taus < 0) || any(taus > 1)) {
		stop("Probabilities (tau) must be between 0 and 1.")
	}
	if (all(taus != 1)) {
		stop("Must include 1 in probabilities (tau).")
	}
	if (!is.numeric(y)) {
		stop("Quantiles must be numeric.")
	}
	if (any(diff(taus) < 0)) {
		stop("Pairs must be increasing to create a valid distribution.")
	}
	id_duplicates <- which(diff(taus) == 0) + 1
	id_zero_tau <- which(taus == 0)
	id_rm <- c(id_duplicates, id_zero_tau)
	mf <- mf[-id_rm, ]
	n <- nrow(mf)
	taus <- mf[[1]]
	y    <- mf[[2]]
	cdf <- stats::stepfun(y, c(0, tau), right = FALSE)
	qf  <- stats::stepfun(tau[-n], y, right = TRUE)
	sf  <- stats::stepfun(y, rev(c(0, tau)), right = FALSE)
	res <- dst(fun_cumu = cdf, fun_quant = qf, ...)
	class(res) <- c("stepdst", class(res))
	res
}
