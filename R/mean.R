#' Get the mean of a distribution
#'
#' @param object Object of class "dst" to obtain mean from.
#' @param ... Arguments to pass to the \code{integrate()} function.
#' @param verbose Print output of \code{integrate()} function?
#' @details If the mean is not already available in the distribution
#' object, this is calculated using the \code{stats::integrate()}
#' function.
#' @return A single numeric
mean.dst <- function(object, ..., verbose = FALSE) {
	mu <- object$prop$mean
	if (!is.null(mu)) return(mu)
	qf <- object$qdst
	int <- stats::integrate(qf, 0, 1)
	if (verbose) print(int)
	int$value
}

mean <- function(...) UseMethod("mean")
