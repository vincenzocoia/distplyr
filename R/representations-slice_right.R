#' @export
eval_cdf.slice_right <- function(object, at) {
	with(object, {
		p_kept <- prob_left(
			distribution, of = breakpoint, inclusive = !include
		)
		cdf <- eval_cdf(distribution, at = at) / p_kept
		pmin(cdf, 1)
	})
}

#' @export
eval_density.slice_right <- function(object, at, strict = TRUE) {
	with(object, {
		p_kept <- prob_left(
			distribution, of = breakpoint, inclusive = !include
		)
		pdf <- eval_density(distribution, at = at, strict = strict) / p_kept
		if (include) {
			pdf[at >= breakpoint] <- 0
		} else {
			pdf[at > breakpoint] <- 0
		}
		pdf
	})
}

#' @export
eval_pmf.slice_right <- function(object, at, strict = TRUE) {
	with(object, {
		p_kept <- prob_left(
			distribution, of = breakpoint, inclusive = !include
		)
		pmf <- eval_pmf(distribution, at = at, strict = strict) / p_kept
		if (include) {
			pmf[at >= breakpoint] <- 0
		} else {
			pmf[at > breakpoint] <- 0
		}
		pmf
	})
}

#' @export
eval_quantile.slice_right <- function(object, at, ...) {
	with(object, {
		p_kept <- prob_left(
			distribution, of = breakpoint, inclusive = !include
		)
		eval_quantile(distribution, at = at * p_kept, ...)
	})
}
