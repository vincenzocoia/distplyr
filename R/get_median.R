#' Median of a Distribution
#'
#' @param object Distribution object
#' @export
median <- function(object) UseMethod("median")

#' @export
median.dst <- function(object) {
	eval_quantile(object, at = 0.5)
}
