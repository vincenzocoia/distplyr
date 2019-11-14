#' Get the mean of a distribution
#'
#' @param x Object of class "dst" to obtain mean from.
#' @param ... Arguments to pass to the \code{integrate()} function.
#' @param verbose Print output of \code{integrate()} function?
#' @details If the mean is not already available in the distribution
#' object, this is calculated using the \code{stats::integrate()}
#' function.
#' @return A single numeric
#' @export
mean.dst <- function(x, ..., verbose = FALSE) {
	mu <- x$prop$mean
	if (!is.null(mu)) return(mu)
	qf <- fun_quant(x)
	int <- stats::integrate(qf, 0, 1)
	if (verbose) print(int)
	int$value
}
