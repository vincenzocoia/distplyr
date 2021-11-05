#' @export
range.slice_left <- function(distribution, ...) {
	# Not accurate when slicing between discrete points.
	r <- distionary::range(distribution$distribution)
	r[1L] <- distribution$breakpoint
	r
}
