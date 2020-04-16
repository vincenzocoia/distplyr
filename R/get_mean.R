#' Mean of a Distribution
#'
#' Get the mean of a distribution.
#'
#' @param object A distribution object from which to obtain the mean.
#' @param ... Arguments to pass to the \code{integrate()} function (if needed).
#' @details If the mean is not already available in the distribution
#' object, and if the distribution is not a step distribution,
#' the mean is calculated using the \code{stats::integrate()}
#' function.
#' @return A single numeric.
#' @rdname get_mean
#' @export
get_mean <- function(object, ...) UseMethod("get_mean")


#' @export
get_mean.dst <- function(object, ...) {
	qf <- get_quantile(object)
	int <- stats::integrate(qf, 0, 1)
	int$value
}


