#' Skewness
#'
#' @param object Distribution object
#' @export
get_skewness <- function(object) UseMethod("get_skewness")

#' @export
get_skewness.dst <- function(object) {
	mu <- get_mean(object)
	sigma <- get_sd(object)
	sf <- get_survival(object)
	sf2 <- function(x) sf(mu + x ^ (1 / 3))
	one_minus <- function(x) 1 - sf2(x)
	pos_int <- integrate(sf2, 0, Inf)
	neg_int <- integrate(one_minus, -Inf, 0)
	pos_int$value - neg_int$value
}
