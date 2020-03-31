#' Empirical Functions for Distributions
#'
#' Makes empirical mass and quantile functions from a univariate
#' sample, just as \code{stats::ecdf()} makes empirical cdf's.
#'
#' @param x Numeric vector of the observations for which to make the quantile
#' function
#' @return Vectorized functions; quantile function for \code{eqf()}, and
#' probability mass function for \code{epmf()}.
#' @rdname efun
#' @export
eqf <- function(x) {
	cdf <- stats::ecdf(x)
	taus <- plateaus(cdf)
	y <- stats::knots(cdf)
	y <- c(y[1], y, y[length(y)]) # Perhaps one day y can be sandwiched by NaN's.
	stats::stepfun(taus, y, right = TRUE)
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


#' Check Continuity of Step Function
#'
#' Check for left and right continuity of
#' a step function.
#'
#' @param object Object of class "stepfun" to check.
#' @return Single logical indicating the result.
#' @export
#' @rdname check_continuous
check_left_continuous <- function(object) {
	if (!stats::is.stepfun(object)) {
		stop("Object being tested is not a step function.")
	}
	f <- with(environment(object), f)
	if (f == 1) TRUE else FALSE
}

#' @export
#' @rdname check_continuous
check_right_continuous <- function(object) {
	if (!stats::is.stepfun(object)) {
		stop("Object being tested is not a step function.")
	}
	f <- with(environment(object), f)
	if (f == 0) TRUE else FALSE
}

#' Extract Heights of a Step Function
#'
#' Extracts the heights/"y values"/plateaus of a
#' step function. Sister function to \code{stats::knots()},
#' which returns the breakpoints/"x values".
#'
#' @param object Object of class "stepfun".
#' @return Vector of y values from left to right.
#' @export
plateaus <- function(object) UseMethod("plateaus")

#' @export
plateaus.stepfun <- function(object) {
	e <- environment(object)
	fval <- with(e, f)
	y <- with(e, y)
	if (fval == 1) {
		yright <- with(e, yright)
		return(c(y, yright))
	} else {
		yleft <- with(e, yleft)
		return(c(yleft, y))
	}
}

#' Swap Direction of Continuity of a Step Function
#'
#' @param stepfun Object of class "stepfun".
#' @return Another stepfun with the continuity swapped
#' between left and right continuous. That is, the only
#' way the new function evaluates differently is at the
#' breakpoints.
#' @export
swap_step_continuity_direction <- function(stepfun) {
	x <- stats::knots(stepfun)
	y <- plateaus(stepfun)
	l <- check_left_continuous(stepfun)
	r <- check_right_continuous(stepfun)
	if (!l && !r) {
		stop("Step function provided is neither left nor right continuous.")
	}
	if (l) {
		stats::stepfun(x, y, right = FALSE)
	} else {
		stats::stepfun(x, y, right = TRUE)
	}
}

#' Consolidate Weights
#'
#' For a vector of outcomes \code{y} with a matching vector of weights,
#' this function provides a single non-zero, non-NA
#' weight per unique value of \code{y}. Weights sum to 1.
#' @param y Vector of outcomes.
#' @param weights Vector of weights, one for each of \code{y}.
#' These need not sum to one, but must not be negative.
#' @return Data frame with the following columns:
#' \enumerate{
#'   \item \code{y}: Increasing vector of unique values of \code{y}
#'   having positive weight.
#'   \item \code{prob}: Weights corresponding to each outcome.
#'   \item \code{tau}: Cumulative weights; that is, \code{cumsum(prob)}.
#' }
#' @export
consolidate_weights <- function(y, weights) {
	stopifnot(identical(length(y), length(weights)))
	yw <- data.frame(y = y, w = weights)
	yw <- stats::na.omit(yw)
	yw <- yw[yw[["w"]] != 0, ]
	yw <- yw[order(yw[["y"]]), ]
	y <- yw[["y"]]
	w <- yw[["w"]]
	w <- w / sum(w)
	tau <- cumsum(w)
	rm_id <- which(duplicated(y)) - 1
	if (length(rm_id) > 0) {
		tau <- tau[-rm_id]
	}
	prob <- diff(c(0, tau))
	stopifnot(sum(prob) == 1)
	y <- unique(y)
	stopifnot(length(y) == length(tau))
	data.frame(y = y, prob = prob, tau = tau)
}
