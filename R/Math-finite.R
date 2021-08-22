#' @export
Math.finite <- function(x) {
	f <- .Generic[[1]]
	call <- rlang::call2(f, expr(location))
	mutate_finite(x, !!call)
}
