#' Conditional Distributions
#'
#' `slice_left()` removes probability to the left of some breakpoint,
#' conditioning the random variable to be bigger than the breakpoint.
#' `slice_right()` does the opposite: removes probability to the right,
#' conditioning to be smaller than the breakpoint.
#'
#' @param object Distribution to slice.
#' @param breakpoint Point at which to slice (single numeric).
#' @param keep_breakpoint Logical; should the breakpoint be kept in the
#' resulting conditional distribution? This is only relevant if the
#' breakpoint has a non-zero probability of occurrence.
#' @export
slice_left <- function(object, breakpoint, keep_breakpoint = FALSE) {
	# Simplification cases
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
	if (breakpoint == right && keep_breakpoint) {
		p <- eval_pmf(object, at = breakpoint, strict = FALSE)
		if (p == 0) {
			stop("No such distribution exists: ",
				 "cannot slice off entire distribution.")
		} else {
			return(dst_degenerate(breakpoint))
		}
	}
	if (is_finite_dst(object)) {
		right_discretes <- next_discrete(object, from = breakpoint, n = Inf,
										 include_from = keep_breakpoint)
		right_probs <- eval_pmf(object, at = right_discretes)
		right_probs <- right_probs / sum(right_probs)
		return(dst_finite(y = right_discretes, probs = right_probs))
	}
	# Create object
	l <- list(
		distribution = object,
		breakpoint = breakpoint,
		keep_breakpoint = keep_breakpoint
	)
	variable <- "unknown" # For now. Need to evaluate cumulative discrete probs.
	structure(l, variable = variable, class = c("slice_left", class(object)))
}

#' @export
eval_cdf.slice_left <- function(object, at) {
	with(object, {
		p_low <- eval_cdf(distribution, at = breakpoint)
		if (keep_breakpoint) {
			p_break <- eval_pmf(distribution, at = breakpoint, strict = FALSE)
			p_low <- p_low - p_break
		}
		cdf <- eval_cdf(distribution, at = at)
		cdf <- (cdf - p_low) / (1 - p_low)
		pmax(cdf, 0)
	})
}

#' @export
eval_survival.slice_left <- function(object, at) {
	with(object, {
		p_high <- eval_survival(distribution, at = breakpoint)
		if (keep_breakpoint) {
			p_break <- eval_pmf(distribution, at = breakpoint, strict = FALSE)
			p_high <- p_high + p_break
		}
		s <- eval_survival(distribution, at = at)
		s <- s / p_high
		pmin(s, 1)
	})
}

#' @export
eval_density.slice_left <- function(object, at, strict = TRUE) {
	with(object, {
		p_high <- eval_survival(distribution, at = breakpoint)
		if (keep_breakpoint) {
			p_break <- eval_pmf(distribution, at = breakpoint, strict = FALSE)
			p_high <- p_high + p_break
		}
		pdf <- eval_density(distribution, at = at, strict = strict)
		pdf <- pdf / p_high
		if (keep_breakpoint) {
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
		p_high <- eval_survival(distribution, at = breakpoint)
		if (keep_breakpoint) {
			p_break <- eval_pmf(distribution, at = breakpoint, strict = FALSE)
			p_high <- p_high + p_break
		}
		pmf <- eval_pmf(distribution, at = at, strict = strict)
		pmf <- pmf / p_high
		if (keep_breakpoint) {
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
		p_high <- eval_survival(distribution, at = breakpoint)
		if (keep_breakpoint) {
			p_break <- eval_pmf(distribution, at = breakpoint, strict = FALSE)
			p_high <- p_high + p_break
		}
		eval_quantile(distribution, at = at * p_high + (1 - p_high), ...)
	})
}
