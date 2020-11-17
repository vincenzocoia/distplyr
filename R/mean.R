#' Moments of a Distribution
#'
#' Get common moment-related quantities of a
#' distribution: mean, variance, standard deviation (sd),
#' skewness, and kurtosis.
#'
#' @param object A distribution object.
#' @param ... Arguments to pass to the \code{integrate()} function (if needed).
#' @details If there is no method associated with a subclass of
#' \code{object}, then moments are calculated using \code{stats::integrate()}
#' from the survival function.
#' @return A single numeric.
#' @examples
#' a <- dst_gpd(0, 1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' mean(a)
#' variance(b)
#' kurtosis_raw(c)
#' kurtosis_exc(c)
#' mean(mix(a, b, probs = c(0.5, 0.5)))
#' @rdname moments
#' @export
mean.dst <- function(object, ...) {
	qf <- get_quantile(object)
	int <- stats::integrate(qf, 0, 1)
	int$value
}


