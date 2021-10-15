#' @export
mean.parametric <- function(x) {
	mean_exists_in_internal_data <- TRUE # Check somehow.
	if (mean_exists_in_internal_data) {
		# grab the mean expression
		mean_as_expression <- rlang::expr(pi) # Get somehow.
		return(rlang::eval_tidy(mean_as_expression))
	}
	mean_exists_with_object <- TRUE # Check if they used something like set_mean
	if (mean_exists_with_object) {
		# grab the mean that's carried along with the object.
		return(x$mean)
	}
	# Otherwise, peel back the "parametric" class and fall back on "dst" method.
	mean.dst(x)
}
