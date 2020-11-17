#' @rdname moments
#' @export
kurtosis_raw <- function(object) UseMethod("kurtosis_raw")

#' @rdname moments
#' @export
kurtosis_exc <- function(object) UseMethod("kurtosis_exc")

#' @export
kurtosis_raw.dst <- function(object) {
	3 + kurtosis_exc(object)
}

#' @export
kurtosis_exc.dst <- function(object) {
	mu <- mean(object)
	var <- variance(object)
	sf <- get_survival(object)
	sf2 <- function(x) 1 + sf(mu + x ^ (1 / 4)) - sf(mu - x ^ (1 / 4))
	int <- stats::integrate(sf2, 0, Inf)
	int$value / var ^ 2 - 3
}
