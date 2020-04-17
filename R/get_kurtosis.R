#' Kurtosis
#'
#' Get the kurtosis (raw) or excess kurtosis (exc) of a distribution.
#'
#' @param object Distribution object
#' @rdname kurtosis
#' @export
get_kurtosis_raw <- function(object) UseMethod("get_kurtosis_raw")

#' @rdname kurtosis
#' @export
get_kurtosis_exc <- function(object) UseMethod("get_kurtosis_exc")

#' @export
get_kurtosis_raw.dst <- function(object) {
	3 + get_kurtosis_exc(object)
}

#' @export
get_kurtosis_exc.dst <- function(object) {
	mu <- get_mean(object)
	var <- get_variance(object)
	sf <- get_survival(object)
	sf2 <- function(x) 1 + sf(mu + x ^ (1 / 4)) - sf(mu - x ^ (1 / 4))
	int <- integrate(sf2, 0, Inf)
	int$value / var ^ 2 - 3
}
