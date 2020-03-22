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
#' @rdname stepdst
#' @export
stepdst <- function(y, data, weights = 1, ...) {
	sy <- substitute(y)
	sw <- substitute(weights)
	if (missing(data)) {
		y <- eval.parent(sy)
		w <- eval.parent(sw)
	} else {
		y <- eval(sy, envir = data)
		w <- eval(sw, envir = data)
	}
	yw <- data.frame(y = y, w = w)
	yw <- stats::na.omit(yw)
	if (any(yw[["w"]] < 0)) {
		stop("Weights must not be negative.")
	}
	if (!is.numeric(y)) {
		stop("Outcomes must be numeric.")
	}
	yw <- yw[yw[["w"]] != 0, ]
	yw <- yw[order(yw[["y"]]), ]
	y <- yw[["y"]]
	w <- yw[["w"]]
	w <- w / sum(w)
	taus <- cumsum(w)
	rm_id <- which(duplicated(y)) - 1
	if (length(rm_id) > 0) taus <- taus[-rm_id]
	taus_w_0 <- c(0, taus)
	probs <- diff(taus_w_0)
	y <- unique(y)
	stopifnot(length(y) == length(taus))
	steps <- data.frame(y = y, tau = taus)
	n <- length(y)
	cdf <- stats::stepfun(y, taus_w_0, right = FALSE)
	qf  <- stats::stepfun(taus[-n], y, right = TRUE)
	sf  <- stats::stepfun(y, 1 - taus_w_0, right = FALSE)
	rf <- function(n) sample(y, size = n, replace = TRUE, prob = probs)
	res <- dst(fun_cumu  = cdf,
			   fun_quant = qf,
			   fun_rand  = rf,
			   fun_surv  = sf,
			   ...)
	structure(res,
			  steps = steps,
			  class = c("stepdst", class(res))
	)
}

#' @rdname stepdst
#' @export
is_stepdst <- function(y) inherits(y, "stepdst")

#' @rdname stepdst
#' @export
is.stepdst <- function(y) inherits(y, "stepdst")

#' Get Step Points from a Step Distribution
#'
#' Step points are the coordinates marking the
#' positions of the step discontinuities in a step
#' distribution. They are the left-most points of
#' each non-zero plateau of the cdf.
#'
#' @param object A step distribution object.
#' @return Data frame with two columns: column
#' \code{y} contains the outcomes corresponding to
#' the step discontinuities of the cdf, and column
#' \code{tau} contains the non-zero plateaus of the
#' cdf (so that the cdf at \code{y} evaluates to
#' \code{tau})
#' @rdname steps
#' @export
steps <- function(object) UseMethod("steps")

#' @export
steps.stepdst <- function(object) {
	attributes(object)[["steps"]]
}

