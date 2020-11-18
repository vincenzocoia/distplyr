#' Median of a Distribution
#'
#' @param object Distribution object
#' @param ... Other arguments to pass to specific methods
#' @export
median.dst <- function(object, ...) {
	eval_quantile(object, at = 0.5)
}
