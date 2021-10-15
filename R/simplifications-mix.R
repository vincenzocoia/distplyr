#' #' @export
#' slice_right.mix <- function(object, breakpoint, include = TRUE, ...) {
#' 	with(object$components, {
#' 		keep <- vapply(distributions, prob_right, FUN.VALUE = numeric(1L),
#' 					   of = breakpoint, inclusive = include) < 1
#' 		if (all(!keep)) {
#' 			stop("No such distribution exists: ",
#' 				 "cannot slice off entire distribution.")
#' 		}
#' 		kept_d <- distributions[keep]
#' 		kept_p <- probs[keep] / sum(probs[keep])
#' 		sliced_d <- lapply(kept_d, slice_right,
#' 						   breakpoint = breakpoint, include = include)
#' 		mix(!!!sliced_d, weights = kept_p)
#' 	})
#' }
#'
#' #' @export
#' slice_left.mix <- function(object, breakpoint, include = TRUE, ...) {
#' 	with(object$components, {
#' 		keep <- vapply(distributions, prob_left, FUN.VALUE = numeric(1L),
#' 					   of = breakpoint, inclusive = include) < 1
#' 		if (all(!keep)) {
#' 			stop("No such distribution exists: ",
#' 				 "cannot slice off entire distribution.")
#' 		}
#' 		kept_d <- distributions[keep]
#' 		kept_p <- probs[keep]
#' 		sliced_d <- lapply(kept_d, slice_left,
#' 						   breakpoint = breakpoint, include = include)
#' 		mix(!!!sliced_d, weights = kept_p)
#' 	})
#' }
