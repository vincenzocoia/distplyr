#' @rdname slice
#' @export
slice_right <- function(object, breakpoint, include = FALSE, ...) {
	UseMethod("slice_right")
}

#' @export
slice_right.dst <- function(object, breakpoint, include = FALSE, ...) {
	rng <- range(object)
	left <- rng[1L]
	right <- rng[2L]
	if (breakpoint > right) {
		return(object)
	}
	if (breakpoint < left) {
		stop("No such distribution exists: ",
			 "cannot slice off entire distribution.")
	}
	if (breakpoint == left && include) {
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
	new_distribution(l, variable = v, class = "slice_right")
}

#' @export
slice_right.finite <- function(object, breakpoint, include = FALSE, ...) {
	left_discretes <- prev_discrete(object, from = breakpoint, n = Inf,
									include_from = include)
	if (!length(left_discretes)) {
		stop("No such distribution exists: ",
			 "cannot slice off entire distribution.")
	}
	left_probs <- eval_pmf(object, at = left_discretes)
	left_probs <- left_probs / sum(left_probs)
	dst_finite(y = left_discretes, probs = left_probs)
}

#' @export
eval_cdf.slice_right <- function(object, at) {
	with(object, {
		p_kept <- prob_left(
			distribution, of = breakpoint, inclusive = include
		)
		cdf <- eval_cdf(distribution, at = at) / p_kept
		pmin(cdf, 1)
	})
}

#' @export
eval_density.slice_right <- function(object, at, strict = TRUE) {
	with(object, {
		p_kept <- prob_left(
			distribution, of = breakpoint, inclusive = include
		)
		pdf <- eval_density(distribution, at = at, strict = strict) / p_kept
		if (include) {
			pdf[at > breakpoint] <- 0
		} else {
			pdf[at >= breakpoint] <- 0
		}
		pdf
	})
}

#' @export
eval_pmf.slice_right <- function(object, at, strict = TRUE) {
	with(object, {
		p_kept <- prob_left(
			distribution, of = breakpoint, inclusive = include
		)
		pmf <- eval_pmf(distribution, at = at, strict = strict) / p_kept
		if (include) {
			pmf[at > breakpoint] <- 0
		} else {
			pmf[at >= breakpoint] <- 0
		}
		pmf
	})
}

#' @export
eval_quantile.slice_right <- function(object, at, ...) {
	with(object, {
		p_kept <- prob_left(
			distribution, of = breakpoint, inclusive = include
		)
		eval_quantile(distribution, at = at * p_kept, ...)
	})
}

#' @export
range.slice_right <- function(object, ...) {
	r <- range(object$distribution)
	r[2L] <- object$breakpoint
	r
}
