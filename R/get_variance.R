#' @rdname moments
#' @export
variance <- function(object, ...) UseMethod("variance")

#' @export
variance.dst <- function(object, ...) {
	mu <- mean(object)
	sf <- get_survival(object)
	sf2 <- function(x) 1 + sf(mu + sqrt(x)) - sf(mu - sqrt(x))
	int <- stats::integrate(sf2, 0, Inf)
	int$value
}


#' @rdname moments
#' @export
sd <- function(object, ...) UseMethod("sd")

#' @export
sd.dst <- function(object, ...) {
	ss <- variance(object, ...)
	sqrt(ss)
}

#' Access to the original sd function
#'
#' Since distplyr makes \code{sd()} a generic function, this
#' default method dispatches the \code{stats::sd()} function
#' whenever \code{sd()} is called on a non-distribution object.
#'
#' @seealso \link{\code{stats::sd}}
#'
#' @export
sd.default <- function(object, ...) {
	stats::sd(object, ...)
}
