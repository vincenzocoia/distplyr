#' @rdname slice
#' @export
slice_right <- function(object, breakpoint, include = TRUE, ...) {
	UseMethod("slice_right")
}

#' @export
slice_right.dst <- function(object, breakpoint, include = TRUE, ...) {
	rng <- range(object)
	left <- rng[1L]
	right <- rng[2L]
	if (breakpoint > right) {
		return(object)
	}
	all_sliced <- FALSE
	if (breakpoint < left) {
		all_sliced <- TRUE
	}
	if (breakpoint == left) {
		if (include) {
			all_sliced <- TRUE
		} else {
			p <- eval_pmf(object, at = breakpoint, strict = FALSE)
			if (p == 0) {
				all_sliced <- TRUE
			} else {
				return(dst_degenerate(breakpoint))
			}
		}
	}
	if (all_sliced) {
		stop("No such distribution exists: ",
			 "cannot slice off entire distribution.")
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
slice_right.finite <- function(object, breakpoint, include = TRUE, ...) {
	left_discretes <- prev_discrete(object, from = breakpoint, n = Inf,
									include_from = !include)
	if (!length(left_discretes)) {
		stop("No such distribution exists: ",
			 "cannot slice off entire distribution.")
	}
	left_probs <- eval_pmf(object, at = left_discretes)
	dst_empirical(left_discretes, weights = left_probs)
}
