#' @export
mean.finite <- function(x, ...) {
	with(x$probabilities, {
		sum(size * location)
	})
}

#' @export
evi.finite <- function(x, ...) {
	NaN
}

#' @export
variance.finite <- function(x, ...) {
	with(x$probabilities, {
		mu <- mean(x)
		mu2 <- sum(size * location^2)
		mu2 - mu^2
	})
}

#' @export
skewness.finite <- function(x, ...) {
	mu <- mean(x)
	sigma <- stdev(x)
	with(x$probabilities, {
		trans <- ((location - mu) / sigma)^3
		sum(trans * size)
	})
}

#' @export
kurtosis_exc.finite <- function(x, ...) {
	mu <- mean(x)
	sigma <- stdev(x)
	with(x$probabilities, {
		trans <- ((location - mu) / sigma)^4
		sum(trans * size) - 3
	})
}

#' @rdname range
#' @export
range.finite <- function(x, ...) {
	unlisted_probability_list <- x$probabilities$location
	min_val <- min(unlisted_probability_list)
	max_val <- max(unlisted_probability_list)
	c(min_val, max_val)
}
