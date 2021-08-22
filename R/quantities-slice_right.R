#' @export
range.slice_right <- function(object, ...) {
	# Not accurate when slicing between discrete points.
	r <- range(object$distribution)
	r[2L] <- object$breakpoint
	r
}
