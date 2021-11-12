#' @export
range.slice_right <- function(distribution, ...) {
	# Not accurate when slicing between discrete points.
	r <- range(distribution$distribution)
	r[2L] <- distribution$breakpoint
	r
}
