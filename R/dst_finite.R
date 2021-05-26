#' Create a Finite Distribution
#'
#' A finite distribution assigns probabilities to a finite
#' collection of values. This includes categorical distributions.
#'
#' @param y <`data-masking`>
#'   Outcomes to comprise the distribution. Should either
#'   evaluate to an (atomic) vector, or be a name in the specified data.
#' @param probs <`data-masking`>
#'   Probabilities corresponding to the outcomes in `y`.
#'   Must not be negative, but **must sum to 1**
#'   (unlike [dst_empirical()]). Should either
#'   evaluate to a vector, or be a name in the specified data.
#' @param data Data frame containing the outcomes `y` and/or
#'   probabilities `probs`. Optional.
#' @param ... Additional arguments, currently not used.
#' @return An object of class `c("finite", "dst")`.
#' @note This distribution is called "finite" and not
#' "discrete", because a discrete distribution could have
#' an infinite amount of possible outcomes, as in the
#' Poisson distribution.
#' @seealso [dst_empirical()]
#' @examples
#' dst_finite(1:5, probs = rep(0.2, 5))
#' @export
dst_finite <- function(y, probs, data, ...) {
	enquo_y <- rlang::enquo(y)
	enquo_w <- rlang::enquo(probs)
	if (missing(data)) {
		y <- rlang::eval_tidy(enquo_y)
		w <- rlang::eval_tidy(enquo_w)
	} else {
		y <- rlang::eval_tidy(enquo_y, data = data)
		w <- rlang::eval_tidy(enquo_w, data = data)
	}
	if (length(y) == 0) {
		warning("Can't make a finite distribution from empty data. ",
				"Returning an empty distribution.")
		return(distribution())
	}
	if (length(w) < length(y)) {
		stop("Not enough probabilities to match outcomes `y`.")
	}
	if (length(w) > length(y)) {
		stop("Not enough outcomes `y` to match probabilities.")
	}
	if (any(w < 0, na.rm = TRUE)) {
		stop("Probabilities must not be negative.")
	}
	if (sum(probs) != 1) {
		stop("Probabilities must add up to 1. ",
			 "Perhaps you'd prefer to use `dst_empirical()`?")
	}
	steps <- aggregate_weights(y, w, sum_to_one = FALSE)
	if (nrow(steps) == 1L) return(dst_degenerate(steps$location))
	res <- list(probabilities = steps)
	new_finite(res, variable = "discrete")
}



#' Create an Empirical Distribution
#'
#' An empirical distribution is a non-parametric way to
#' estimate a distribution using data. By default,
#' it assigns equal probability to all observations
#' (this can be overridden with the `weights` argument).
#' Identical to [dst_finite()] with weights as probabilities,
#' except weights need not add to 1.
#'
#' @param y <`data-masking`>
#'   Outcomes to comprise the distribution. Should either
#'   evaluate to an (atomic) vector, or be a name in the specified data.
#' @param data Data frame containing the outcomes `y` and/or
#'   `weights`. Optional.
#' @param weights <`data-masking`>
#'   Weights to assign each outcome in `y`. Will be
#'   normalized so that the weights add up to 1
#'   (unlike [dst_finite()]),
#'   representing probabilities.
#' @param ... Additional arguments, currently not used.
#' @return An object of class `c("finite", "dst")`.
#' @seealso [dst_finite()]
#' @examples
#' require(graphics)
#' require(datasets)
#' marg <- dst_empirical(hp, data = mtcars)
#' plot(marg, "cdf", n = 1001)
#'
#' K <- function(x) dnorm(x, sd = 25)
#' cond <- dst_empirical(hp, data = mtcars, weights = K(disp - 150))
#' plot(cond, "cdf", n = 1001, lty = 2, add = TRUE)
#' @export
dst_empirical <- function(y, data, weights = 1, ...) {
	enquo_y <- rlang::enquo(y)
	enquo_w <- rlang::enquo(weights)
	if (missing(data)) {
		y <- rlang::eval_tidy(enquo_y)
		w <- rlang::eval_tidy(enquo_w)
	} else {
		y <- rlang::eval_tidy(enquo_y, data = data)
		w <- rlang::eval_tidy(enquo_w, data = data)
	}
	if (length(y) == 0L) {
		warning("Can't make an empirical distribution from empty data. ",
				"Returning an empty distribution.")
		return(distribution())
	}
	if (any(w < 0, na.rm = TRUE)) {
		stop("Weights must not be negative.")
	}
	if (length(w) == 1L) {
		w <- rep(w, length(y))
	}
	if (length(w) < length(y)) {
		stop("Not enough weights to match outcomes `y`.")
	}
	if (length(w) > length(y)) {
		stop("Not enough outcomes `y` to match weights.")
	}
	steps <- aggregate_weights(y, w, sum_to_one = TRUE)
	if (nrow(steps) == 1L) return(dst_degenerate(steps$location))
	res <- list(probabilities = steps)
	new_finite(res, variable = "discrete")
}


#' Constructor Function for Finite Distributions
#'
#' @param l List containing the components of a step distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#' or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_finite <- function(l, variable, ..., class = character()) {
	new_distribution(
		l,
		variable = variable,
		...,
		class    = c(class, "finite")
	)
}

#' Is a distribution a finite distribution?
#'
#' There's no difference between checking whether a distribution
#' is finite or empirical -- the functions exist simply for
#' completeness.
#'
#' @param object Object to check
#' @rdname is_finite
#' @export
is_finite_dst <- function(object) inherits(object, "finite")

#' @rdname is_finite
#' @export
is.finite_dst <- function(object) inherits(object, "finite")

#' @rdname is_finite
#' @export
is.empirical <- function(object) inherits(object, "finite")

#' @rdname is_finite
#' @export
is_empirical <- function(object) inherits(object, "finite")



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
		mu2 <- sum(size * location ^ 2)
		mu2 - mu ^ 2
	})
}


#' @export
get_cdf.finite <- function(object) {
	with(object$probabilities, {
		heights <- c(0, cumsum(size))
		stats::stepfun(location, heights, right = FALSE)
	})
}

#' @export
eval_cdf.finite <- function(object, at) {
	get_cdf(object)(at)
}

#' @export
get_survival.finite <- function(object) {
	with(object$probabilities, {
		heights <- 1 - c(0, cumsum(size))
		stats::stepfun(location, heights, right = FALSE)
	})
}

#' @export
eval_survival.finite <- function(object, at) {
	get_survival(object)(at)
}

#' @export
get_quantile.finite <- function(object, ...) {
	with(object$probabilities, {
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
eval_quantile.finite <- function(object, at, ...) {
	get_quantile(object)(at)
}

#' @export
realise.finite <- function(object, n = 1, ...) {
	with(object$probabilities, {
		sample(location, size = n, replace = TRUE, prob = size)
	})
}

#' @export
eval_pmf.finite <- function(object, at) {
	with(object$probabilities, {
		vapply(at, function(x) sum(size[x == location]), FUN.VALUE = numeric(1L))
	})
}

#' @rdname range
#' @export
range.finite <- function(x, ...) {
  unlisted_probability_list <- unlist(x$probabilities$location)
  min_val <- min(unlisted_probability_list)
  max_val <- max(unlisted_probability_list)
  c(min_val, max_val)
}

#' @rdname discontinuities
#' @export
discontinuities.finite <- function(object, from, to, ...) {
  probabilities <- object$probabilities
  location <- probabilities$location
  from_filtered <- probabilities[location >= from, ]
  res <- from_filtered[location <= to, ]
  res
}
