#' Create a Step Distribution
#'
#' A step distribution is one where the cdf
#' and quantile function are step functions.
#' This includes empirical distributions.
#' \code{stepdst()} facilitates the creation of
#' such a distribution by specifying the
#' observations/breakpoints, along with their weights.
#'
#' @param y Outcomes to comprise the distribution. Should either
#' evaluate to a vector, or be a name in the specified data.
#' @param data Data frame, list, or environment
#' containing the outcome name in \code{y}. If missing,
#' \code{y} will be evaluated in the parent frame.
#' @param weights Weights corresponding to the outcomes in \code{y}.
#' Must not be negative, but need not sum to 1. If \code{data}
#' is provided, the data will be searched for the name provided in
#' this argument.
#' @param variable Type of random variable: "continuous", "discrete",
#' or "mixed".
#' @param ... Additional arguments to be passed to \code{\link{dst}}.
#' @return A "stepdst" object, which is also a "dst" object,
#' containing a cdf, quantile function, and random number generator.
#' The cdf is a right-continuous step function, and the quantile function is
#' a left-continuous step function.
#' If you'd like to add other
#' functions through the \code{\link{dst}} function, you can do so
#' via \code{...}.
#' @rdname stepdst
#' @export
stepdst <- function(y, data, weights = 1,
					variable = c("continuous", "discrete", "mixed"),
					...) {
	v <- match.arg(variable)
	sy <- substitute(y)
	sw <- substitute(weights)
	if (missing(data)) {
		y <- eval.parent(sy)
		w <- eval.parent(sw)
	} else {
		y <- eval(sy, envir = data)
		w <- eval(sw, envir = data)
	}
	if (any(w < 0, na.rm = TRUE)) {
		stop("Weights must not be negative.")
	}
	if (!is.numeric(y)) {
		stop("Outcomes must be numeric.")
	}
	if (length(y) == 0) {
		warning("Can't make a step distribution from empty data. ",
				"Returning NULL.")
		return(NULL)
	}
	if (length(w) == 1) {
		w <- rep(w, length(y))
	}
	steps <- aggregate_weights(y, w)
	res <- list(discontinuities = steps)
	new_stepdst(res, variable = v)
}

#' Constructor Function for Step Distributions
#'
#' @param l List containing the components of a step distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#' or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_stepdst <- function(l, variable, ..., class = character()) {
	new_dst(
		l,
		variable = variable,
		...,
		class    = c(class, "stepdst")
	)
}

#' @rdname stepdst
#' @export
is_stepdst <- function(y) inherits(y, "stepdst")

#' @rdname stepdst
#' @export
is.stepdst <- function(y) inherits(y, "stepdst")


#' @export
get_mean.stepdst <- function(object, ...) {
	with(discontinuities(object), {
		sum(size * location)
	})
}

#' @export
get_variance.stepdst <- function(object, ...) {
	with(discontinuities(object), {
		mu <- get_mean(object)
		mu2 <- sum(size * location^2)
		mu2 - mu^2
	})
}


#' @export
get_cdf.stepdst <- function(object) {
	with(discontinuities(object), {
		heights <- c(0, cumsum(size))
		stats::stepfun(location, heights, right = FALSE)
	})
}

#' @export
get_survival.stepdst <- function(object) {
	with(discontinuities(object), {
		heights <- 1 - c(0, cumsum(size))
		stats::stepfun(location, heights, right = FALSE)
	})
}

#' @export
get_quantile.stepdst <- function(object, ...) {
	with(discontinuities(object), {
		if (identical(length(location), 1L)) {
			function(x) {
				x[!is.na(x) & !is.nan(x)] <- location
				x
			}
		} else {
			taus <- cumsum(size)
			taus <- taus[-length(taus)]
			stats::stepfun(taus, location, right = TRUE)
		}
	})


}

#' @export
get_randfn.stepdst <- function(object) {
	with(discontinuities(object), {
		function(n) sample(location, size = n, replace = TRUE, prob = size)
	})
}

#' @export
get_probfn.stepdst <- function(object) {
	if (identical(variable(object), "discrete")) {
		with(discontinuities(object), {
			Vectorize(function(x) sum(size[x == location]))
		})
	}
}

