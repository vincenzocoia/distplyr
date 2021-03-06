#' @rdname moments
#' @export
variance <- function(x, ...) {
	UseMethod("variance")
}

#' @export
variance.dst <- function(x, ...) {
	mu <- mean(x)
	sf <- get_survival(x)
	sf2 <- function(t) 1 + sf(mu + sqrt(t)) - sf(mu - sqrt(t))
	int <- stats::integrate(sf2, 0, Inf)
	int$value
}
