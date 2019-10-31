#' Get the variance/standard deviation of a distribution
#'
#' @param object Object of class "dst" to obtain statistic from.
#' @param ... Arguments to pass to the \code{integrate()} function.
#' @param verbose Print output of \code{integrate()} function?
#' @details If the statistic is not already available in the distribution
#' object, this is calculated using the \code{stats::integrate()}
#' function.
#' @return A single numeric
#' @rdname var
#' @export
var.dst <- function(object, ..., verbose = FALSE) {
	ss <- object$prop$var
	if (!is.null(ss)) {
		return(ss)
	} else {
		stop("Code for calculating variance not available yet.")
	}
}

#' @export
var <- function(object, ..., verbose = FALSE) UseMethod("var")

#' @rdname var
#' @export
sd.dst <- function(object, ..., verbose = FALSE) {
	ss <- var(object, ..., verbose = verbose)
	sqrt(ss)
}

#' @export
sd <- function(object, ..., verbose = FALSE) UseMethod("sd")

# Prevent stats::sd() and stats::var() from breaking

#' @export
sd.numeric <- function(...) stats::sd(...)

#' @export
var.numeric <- function(...) stats::var(...)

#' @export
var.data.frame <- function(...) stats::var(...)
