#' Conditional Distributions
#'
#' `slice_left()` removes probability to the left of some breakpoint,
#' conditioning the random variable to be bigger than the breakpoint.
#' `slice_right()` does the opposite: removes probability to the right,
#' conditioning to be smaller than the breakpoint.
#'
#' @param object Distribution to slice.
#' @param breakpoint Point at which to slice (single numeric).
#' @param include Logical; should the breakpoint be removed as well?
#' This is only realistically relevant if the
#' breakpoint has a non-zero probability of occurrence.
#' @param ... Other arguments to pass to specific methods. Currently unused.
#' @return A conditional distribution.
#' @examples
#' library(magrittr)
#' dst_norm(0, 1) %>%
#'     slice_left(-2) %>%
#'     slice_right(2) %>%
#'     enframe_cdf(at = -3:3)
#'
#' d <- dst_empirical(c(2, 5, 6, 9, 11))
#' d %>%
#'     slice_left(5) %>%
#'     eval_pmf(at = 5)
#' d %>%
#'     slice_left(5, include = TRUE) %>%
#'     eval_pmf(at = 5)
#' @rdname slice
#' @export
slice_left <- function(object, breakpoint, include = TRUE, ...) {
    UseMethod("slice_left")
}

#' @export
slice_left.dst <- function(object, breakpoint, include = TRUE, ...) {
    rng <- range(object)
    left <- rng[1L]
    right <- rng[2L]
    if (breakpoint < left) {
        return(object)
    }
    all_sliced <- FALSE
    if (breakpoint > right) {
        all_sliced <- TRUE
    }
    if (breakpoint == right) {
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
        stop(
            "No such distribution exists: ",
            "cannot slice off entire distribution."
        )
    }
    # if (breakpoint == right && !include) {
    # 	p <- eval_pmf(object, at = breakpoint, strict = FALSE)
    # 	if (p == 0) {
    # 		stop("No such distribution exists: ",
    # 			 "cannot slice off entire distribution.")
    # 	} else {
    # 		return(dst_degenerate(breakpoint))
    # 	}
    # }
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
slice_left.finite <- function(object, breakpoint, include = TRUE, ...) {
    right_discretes <- next_discrete(object,
        from = breakpoint, n = Inf,
        include_from = !include
    )
    if (!length(right_discretes)) {
        stop(
            "No such distribution exists: ",
            "cannot slice off entire distribution."
        )
    }
    right_probs <- eval_pmf(object, at = right_discretes)
    dst_empirical(right_discretes, weights = right_probs)
}