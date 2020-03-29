#' Median of a Distribution
#'
#' @param object Distribution object
#' @export
get_median <- function(object) UseMethod("get_median")

#' @export
get_median.dst <- function(object) {
	eval_quantile(object, at = 0.5)
}
