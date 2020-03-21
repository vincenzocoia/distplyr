#' Create a Step Distribution
#'
#' A step distribution is one where the cdf
#' and quantile function are step functions.
#' This includes empirical distributions.
#' \code{stepdst()} facilitates the creation of
#' such a distribution by specifying the
#' observations/breakpoints, along with their weights.
#'
#' @param y Outcomes to comprise the distribution. Should either
#' evaluate to a vector, or be a name in the specified data.
#' @param data Data frame, list, or environment
#' containing the outcome name in \code{y}. If missing,
#' \code{y} will be evaluated in the parent frame.
#' @param weights Weights corresponding to the outcomes in \code{y}.
#' Must not be negative, but need not sum to 1. If \code{data}
#' is provided, the data will be searched for the name provided in
#' this argument.
#' @param ... Additional arguments to be passed to \code{\link{dst}}.
#' @return A "stepdst" object, which is also a "dst" object,
#' containing a cdf, quantile function, and random number generator.
#' The cdf is a right-continuous step function, and the quantile function is
#' a left-continuous step function.
#' If you'd like to add other
#' functions through the \code{\link{dst}} function, you can do so
#' via \code{...}.
#' @export
stepdst <- function(y, data, weights = 1, ...) {
	sy <- substitute(y)
	sw <- substitute(weights)
	if (missing(data)) {
		y <- eval.parent(sy)
		w <- eval.parent(sw)
	} else {
		y <- eval(sy, envir = data)
		w <- eval(se, envir = data)
	}
	if (any(w < 0)) {
		stop("Weights must not be negative.")
	}
	if (!is.numeric(y)) {
		stop("Outcomes must be numeric.")
	}
	w <- w / sum(w)
	yw <- data.frame(y = y, w = w)
	yw <- na.omit(yw)
	yw <- yw[yw[["w"]] != 0, ]
	yw <- yx[order(yw[["y"]]), ]
	y <- yw[["y"]]
	w <- yw[["w"]]
	taus <- cumsum(w)
	rm_id <- which(duplicated()) - 1
	taus <- taus[-rm_id]
	taus_w_0 <- c(0, taus)
	probs <- diff(taus_w_0)
	y <- unique(y)
	stopifnot(length(y) == length(taus))
	n <- length(y)
	cdf <- stats::stepfun(y, c(0, taus), right = FALSE)
	qf  <- stats::stepfun(taus[-n], y, right = TRUE)
	sf  <- stats::stepfun(y, rev(c(0, taus)), right = FALSE)
	rf <- function(n) sample(y, size = n, replace = TRUE, prob = probs)
	res <- dst(fun_cumu = cdf, fun_quant = qf, fun_rand = rf, ...)
	class(res) <- c("stepdst", class(res))
	res
}
