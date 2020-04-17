#' @rdname moments
#' @export
get_variance <- function(object, ...) UseMethod("get_variance")

#' @export
get_variance.dst <- function(object, ...) {
	mu <- get_mean(object)
	sf <- get_survival(object)
	sf2 <- function(x) 1 + sf(mu + sqrt(x)) - sf(mu - sqrt(x))
	int <- stats::integrate(sf2, 0, Inf)
	int$value
}


#' @rdname moments
#' @export
get_sd <- function(object, ...) UseMethod("get_sd")

#' @export
get_sd.dst <- function(object, ...) {
	ss <- get_variance(object, ...)
	sqrt(ss)
}
