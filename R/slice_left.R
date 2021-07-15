#' Conditional Distributions
#'
#' `slice_left()` removes probability to the left of some breakpoint,
#' conditioning the random variable to be bigger than the breakpoint.
#' `slice_right()` does the opposite: removes probability to the right,
#' conditioning to be smaller than the breakpoint.
#'
#' @param object Distribution to slice.
#' @param breakpoint Point at which to slice (single numeric).
#' @param include Logical; should the breakpoint be kept in the
#' resulting conditional distribution? This is only relevant if the
#' breakpoint has a non-zero probability of occurrence.
#' @param ... Other arguments to pass to specific methods. Currently unused.
#' @return A conditional distribution.
#' @examples
#' library(magrittr)
#' dst_norm(0, 1) %>%
#'   slice_left(-2) %>%
#'   slice_right(2) %>%
#'   enframe_cdf(at = -3:3)
#'
#' d <- dst_empirical(c(2, 5, 6, 9, 11))
#' d %>%
#'   slice_left(5) %>%
#'   eval_pmf(at = 5)
#' d %>%
#'   slice_left(5, include = TRUE) %>%
#'   eval_pmf(at = 5)
#' @rdname slice
#' @export
slice_left <- function(object, breakpoint, include = FALSE, ...) {
	UseMethod("slice_left")
}

#' @export
slice_left.dst <- function(object, breakpoint, include = FALSE, ...) {
	rng <- range(object)
	left <- rng[1L]
	right <- rng[2L]
	if (breakpoint < left) {
		return(object)
	}
	if (breakpoint > right) {
		stop("No such distribution exists: ",
			 "cannot slice off entire distribution.")
	}
	if (breakpoint == right && include) {
		p <- eval_pmf(object, at = breakpoint, strict = FALSE)
		if (p == 0) {
			stop("No such distribution exists: ",
				 "cannot slice off entire distribution.")
		} else {
			return(dst_degenerate(breakpoint))
		}
	}
	l <- list(
		distribution = object,
		breakpoint = breakpoint,
		include = include
	)
	v <- variable(object)
	if (v == "mixed") {
		v <- "unknown" # For now. Need to evaluate cumulative discrete probs.
	}
	new_distribution(l, variable = v, class = "slice_left")
}

#' @export
slice_left.finite <- function(object, breakpoint, include = FALSE, ...) {
	right_discretes <- next_discrete(object, from = breakpoint, n = Inf,
									 include_from = include)
	if (!length(right_discretes)) {
		stop("No such distribution exists: ",
			 "cannot slice off entire distribution.")
	}
	right_probs <- eval_pmf(object, at = right_discretes)
	right_probs <- right_probs / sum(right_probs)
	dst_finite(y = right_discretes, probs = right_probs)
}


#' @export
eval_cdf.slice_left <- function(object, at) {
	with(object, {
		p_kept <- prob_right(
			distribution, of = breakpoint, inclusive = include
		)
		cdf <- 1 - eval_survival(distribution, at = at) / p_kept
		pmax(cdf, 0)
	})
}

#' @export
eval_survival.slice_left <- function(object, at) {
	with(object, {
		p_kept <- prob_right(
			distribution, of = breakpoint, inclusive = include
		)
		s <- eval_survival(distribution, at = at) / p_kept
		pmin(s, 1)
	})
}

#' @export
eval_density.slice_left <- function(object, at, strict = TRUE) {
	with(object, {
		p_kept <- prob_right(
			distribution, of = breakpoint, inclusive = include
		)
		pdf <- eval_density(distribution, at = at, strict = strict) / p_kept
		if (include) {
			pdf[at < breakpoint] <- 0
		} else {
			pdf[at <= breakpoint] <- 0
		}
		pdf
	})
}

#' @export
eval_pmf.slice_left <- function(object, at, strict = TRUE) {
	with(object, {
		p_kept <- prob_right(
			distribution, of = breakpoint, inclusive = include
		)
		pmf <- eval_pmf(distribution, at = at, strict = strict) / p_kept
		if (include) {
			pmf[at < breakpoint] <- 0
		} else {
			pmf[at <= breakpoint] <- 0
		}
		pmf
	})
}

#' @export
eval_quantile.slice_left <- function(object, at, ...) {
	with(object, {
		p_kept <- prob_right(
			distribution, of = breakpoint, inclusive = include
		)
		eval_quantile(distribution, at = (1 - p_kept) + at * p_kept, ...)
	})
}

#' @export
range.slice_left <- function(object, ...) {
	r <- range(object$distribution)
	r[1L] <- object$breakpoint
	r
}
