#' Generalized Pareto Distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param location,scale,shape Parameters of the GPD.
#' @return Object of class "dst" of a GPD.
#' @examples
#' require(graphics)
#' d <- dst_gpd(0, 1, 1)
#' plot(d, "survival", to = 20)
#' @export
dst_gpd <- function(location, scale, shape) {
	if (scale == 0) return(dst_degenerate(location))
	if (scale < 0) stop("'scale' parameter must be non-negative.")
	res <- list(parameters = list(location = location,
								  scale    = scale,
								  shape    = shape))
	new_parametric(res, variable = "continuous", class = "gpd")
}


#' @export
mean.gpd <- function(x, ...) {
	with(
		parameters(x),
		ifelse(shape < 1,
			   location + scale / (1 - shape),
			   Inf)
	)
}


#' @export
variance.gpd <- function(x, ...) {
	with(
		parameters(x),
		ifelse(shape < 1 / 2,
			   scale ^ 2 / (1 - shape) ^ 2 / (1 - 2 * shape),
			   Inf)
	)
}


#' @export
evi.gpd <- function(x, ...) {
	with(parameters(x), shape)
}

#' @export
skewness.gpd <- function(x, ...) {
	with(
		parameters(x),
		ifelse(
			shape < 1 / 3,
			2 * (1 + shape) * sqrt(1 - 2 * shape) /
				(1 - 3 * shape),
			Inf
		)
	)
}

#' @export
kurtosis_exc.gpd <- function(x, ...) {
	with(
		parameters(x),
		ifelse(
			shape < 1 / 4,
			3 * (1 - 2 * shape) * (2 * shape ^ 2 + shape + 3) /
				((1 - 3 * shape) * (1 - 4 * shape)) - 3,
			Inf
		)
	)
}

#' @export
eval_cdf.gpd <- function(object, at) {
	with(parameters(object), {
		if (shape == 0) {
			left <- at < location
			z <- (at - location) / scale
			res <- 1 - exp(-z)
			res[left] <- 0
			res
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			left <- at < location
			right <- at > rightend
			z <- (at - location) / scale
			res <- 1 - (1 + shape * z) ^ (-1 / shape)
			res[left] <- 0
			res[right] <- 1
			res
		}
	})
}

#' @export
eval_quantile.gpd <- function(object, at, ...) {
	with(parameters(object), {
		invalid <- at < 0 | at > 1
		if (shape == 0) {
			res <- location - scale * log(1 - at)
			res[invalid] <- NaN
			res
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			t <- 1 / (1 - at)
			res <- location + scale * (t ^ shape - 1) / shape
			res[invalid] <- NaN
			res
		}
	})
}

#' @export
eval_density.gpd <- function(object, at) {
	with(parameters(object), {
		z <- (at - location) / scale
		if (shape == 0) {
			outside <- at < location
			res <- exp(-z) / scale
			res[outside] <- 0
			res
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			outside <- at < location | at > rightend
			res <- (1 + shape * z) ^ (-1 / shape - 1) / scale
			res[outside] <- 0
			res
		}
	})
}

#' @rdname range
#' @export
range.gpd <- function(x, ...) {
	return(c(parameters(x)$location, Inf))
}




# Using .dst method for:
#
# - median
# - get_survival
# - get_hazard
# - get_chf
