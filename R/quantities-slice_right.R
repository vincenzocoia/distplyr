#' @export
range.slice_right <- function(distribution, ...) {
	ellipsis::check_dots_empty()
	# Not accurate when slicing between discrete points.
	r <- distionary::range(distribution$distribution)
	r[2L] <- distribution$breakpoint
	r
}
