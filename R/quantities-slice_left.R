#' @export
range.slice_left <- function(object, ...) {
	# Not accurate when slicing between discrete points.
	r <- range(object$distribution)
	r[1L] <- object$breakpoint
	r
}
