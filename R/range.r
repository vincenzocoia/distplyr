#' @title Range of Distribution
#' @description Generic function to find range of distributions

#' @export
range <- function(x, ...) {
	UseMethod("range")

}

#' @export
range.dst <- function(x, ...) {
	minVal <- eval_quantile(x, at = 0)
	maxVal <- eval_quantile(x, at = 1)
	return(c(minVal, maxVal))

}
