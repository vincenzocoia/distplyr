#' @export
Ops.finite <- function(e1, e2) {
	op <- .Generic[[1]]
	if (is_distribution(e1) && is_distribution(e2)) {
		stop("Operations on two distributions not currently supported.")
	}
	if (is_distribution(e1)) {
		call <- rlang::call2(op, expr(location), e2)
		mutate_finite(e1, !!call)
	} else {
		call <- rlang::call2(op, e1, expr(location))
		mutate_finite(e2, !!call)
	}
}
