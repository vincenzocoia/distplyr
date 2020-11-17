#' @rdname moments
#' @export
skewness <- function(object) UseMethod("skewness")

#' @export
skewness.dst <- function(object) {
	mu <- mean(object)
	sigma <- sd(object)
	sf <- get_survival(object)
	sf2 <- function(x) sf(mu + x ^ (1 / 3))
	one_minus_flipped <- function(x) 1 - sf(mu - x ^ (1 / 3))
	# (flipped about x=0 because (-1)^(1/3) returns a complex root of
	#  unity, or NaN, instead of the real one, -1.)
	pos_int <- stats::integrate(sf2, 0, Inf)
	neg_int <- stats::integrate(one_minus_flipped, 0, Inf)
	(pos_int$value - neg_int$value) / sigma ^ 3
}
