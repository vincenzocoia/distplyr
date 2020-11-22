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
#'   evaluate to a vector, or be a name in the specified data.
#' @param data Data frame, list, or environment
#'   containing the outcome name in \code{y}. If missing,
#'   \code{y} will be evaluated in the parent frame.
#' @param weights Weights corresponding to the outcomes in \code{y}.
#'   Must not be negative, but need not sum to 1. If \code{data}
#'   is provided, the data will be searched for the name provided in
#'   this argument.
#' @param ... Additional arguments, currently not used.
#' @return A "stepdst" object, which is also a "dst" object.
#'   The cdf is a right-continuous step function,
#'   and the quantile function is a left-continuous step function.
#' @rdname stepdst
#' @examples
#' require(graphics)
#' require(datasets)
#' marg <- stepdst(hp, data = mtcars)
#' plot(marg, "cdf", n = 1001)
#'
#' K <- function(x) dnorm(x, sd = 25)
#' cond <- stepdst(hp, data = mtcars, weights = K(disp - 150))
#' plot(cond, "cdf", n = 1001, lty = 2, add = TRUE)
#' @export
stepdst <- function(y, data, weights = 1, ...) {
	enquo_y <- rlang::enquo(y)
	enquo_w <- rlang::enquo(weights)
	if (missing(data)) {
		y <- rlang::eval_tidy(enquo_y)
		w <- rlang::eval_tidy(enquo_w)
	} else {
		y <- rlang::eval_tidy(enquo_y, data)
		w <- rlang::eval_tidy(enquo_w, data)
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
	steps <- aggregate_weights(y, w, sum_to_one = TRUE)
	if (nrow(steps) == 1L) return(dst_degenerate(steps$location))
	res <- list(name = "Step", discontinuities = steps)
	new_stepdst(res, variable = "discrete")
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
	new_distribution(
		l,
		variable = variable,
		...,
		class    = c(class, "stepdst")
	)
}

#' @param object Object to check
#' @rdname stepdst
#' @export
is_stepdst <- function(object) inherits(object, "stepdst")

#' @rdname stepdst
#' @export
is.stepdst <- function(object) inherits(object, "stepdst")


#' @export
mean.stepdst <- function(x, ...) {
	with(discontinuities(x), {
		sum(size * location)
	})
}

#' @export
evi.stepdst <- function(x, ...) {
	NaN
}

#' @export
variance.stepdst <- function(x, ...) {
	with(discontinuities(x), {
		mu <- mean(x)
		mu2 <- sum(size * location ^ 2)
		mu2 - mu ^ 2
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
eval_cdf.stepdst <- function(object, at) {
	get_cdf(object)(at)
}

#' @export
get_survival.stepdst <- function(object) {
	with(discontinuities(object), {
		heights <- 1 - c(0, cumsum(size))
		stats::stepfun(location, heights, right = FALSE)
	})
}

#' @export
eval_survival.stepdst <- function(object, at) {
	get_survival(object)(at)
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
eval_quantile.stepdst <- function(object, at, ...) {
	get_quantile(object)(at)
}

#' @export
realise.stepdst <- function(object, n = 1, ...) {
	with(discontinuities(object), {
		sample(location, size = n, replace = TRUE, prob = size)
	})
}

#' @export
eval_pmf.stepdst <- function(object, at) {
	with(discontinuities(object), {
		vapply(at, function(x) sum(size[x == location]), FUN.VALUE = numeric(1L))
	})
}
