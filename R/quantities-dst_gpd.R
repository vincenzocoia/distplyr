#' @rdname range
#' @export
range.gpd <- function(x, ...) {
	with(parameters(x), {
		if (shape >= 0) {
			c(location, Inf)
		} else {
			max_val <- location -
				(scale / shape)
			c(location, max_val)
		}
	})
}

#' @export
mean.gpd <- function(x, ...) {
	with(parameters(x), {
		ifelse(shape < 1,
			   location + scale / (1 - shape),
			   Inf
		)
	})
}


#' @export
variance.gpd <- function(x, ...) {
	with(parameters(x), {
		ifelse(shape < 1 / 2,
			   scale^2 / (1 - shape)^2 / (1 - 2 * shape),
			   Inf
		)
	})
}


#' @export
evi.gpd <- function(x, ...) {
	with(parameters(x), shape)
}

#' @export
skewness.gpd <- function(x, ...) {
	with(parameters(x), {
		ifelse(shape < 1 / 3,
			   2 * (1 + shape) * sqrt(1 - 2 * shape) /
			   	(1 - 3 * shape),
			   Inf
		)
	})
}

#' @export
kurtosis_exc.gpd <- function(x, ...) {
	with(parameters(x), {
		ifelse(
			shape < 1 / 4,
			3 * (1 - 2 * shape) * (2 * shape^2 + shape + 3) /
				((1 - 3 * shape) * (1 - 4 * shape)) - 3,
			Inf
		)
	})
}
