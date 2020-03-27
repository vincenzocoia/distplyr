#' Generalized Pareto Distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param location,scale,shape Parameters of the GPD.
#' @return Object of class "dst" of a GPD.
#' @export
dst_gpd <- function(location, scale, shape) {
	if (scale == 0) return(dst_degenerate(location))
	if (scale < 0) stop("'scale' parameter must be non-negative.")
	res <- list(params = list(location = location,
							  scale    = scale,
							  shape    = shape))
	new_parametric_dst(res, variable = "continuous", class = "gpd")
}


#' @export
get_mean.gpd <- function(object) {
	with(
		parameters(object),
		ifelse(shape < 1,
			   location + scale / (1 - shape),
			   Inf)
	)
}


#' @export
get_variance.gpd <- function(object) {
	with(
		parameters(object),
		ifelse(shape < 1 / 2,
			   scale ^ 2 / (1 - shape) ^ 2 / (1 - 2 * shape),
			   Inf)
	)
}


#' @export
get_evi.gpd <- function(object) {
	with(parameters(object), shape)
}

#' @export
get_skewness.gpd <- function(object) {
	with(
		parameters(object),
		ifelse(
			shape < 1 / 3,
			2 * (1 + shape) * sqrt(1 - 2 * shape) /
				(1 - 3 * shape),
			Inf
		)
	)
}

#' @export
get_kurtosis_exc.gpd <- function(object) {
	with(
		parameters(object),
		ifelse(
			shape < 1 / 4,
			3 * (1 - 2 * shape) * (2 * shape ^ 2 + shape + 3) /
				((1 - 3 * shape) * (1 - 4 * shape)) - 3,
			Inf
		)
	)
}

#' @export
get_cdf.gpd <- function(object) {
	with(
		parameters(object),
		if (shape == 0) {
			function(x) {
				left <- x < location
				z <- (x - location) / scale
				res <- 1 - exp(-z)
				res[left] <- 0
				res
			}
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			function(x) {
				left <- x < location
				right <- x > rightend
				z <- (x - location) / scale
				res <- 1 - (1 + shape * z) ^ (-1 / shape)
				res[left] <- 0
				res[right] <- 1
				res
			}
		}
	)
}

#' @export
get_quantile.gpd <- function(object) {
	with(
		parameters(object),
		if (shape == 0) {
			function(x) {
				invalid <- x < 0 | x > 1
				res <- location - scale * log(1 - x)
				res[invalid] <- NaN
				res
			}
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			function(x) {
				invalid <- x < 0 | x > 1
				t <- 1 / (1 - x)
				res <- location + scale * (t ^ shape - 1) / shape
				res[invalid] <- NaN
				res
			}
		}
	)
}

#' @export
get_probfn.gpd <- function(object) {
	with(
		parameters(object),
		if (shape == 0) {
			function(x) {
				outside <- x < location
				z <- (x - location) / scale
				res <- exp(-z) / scale
				res[outside] <- 0
				res
			}
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			function(x) {
				outside <- x < location | x > rightend
				z <- (x - location) / scale
				res <- (1 + shape * z) ^ (-1 / shape - 1) / scale
				res[outside] <- 0
				res
			}
		}
	)
}


# Using .dst method for:
#
# - get_median
# - get_survival
# - get_randfn
# - get_hazard
# - get_chf
