#' Log Normal Distribution
#'
#' Makes a distribution belonging to the family of
#' Log Normal distributions.
#' @param meanlog,variancelog Mean and variance of the distribution
#' on a log scale.
#' @return Object of class `c("lnorm", "dst")`.
#' dst_lnorm(0, 1)
#' @export
dst_lnorm <- function(meanlog, variancelog) {
	if (!inherits(variancelog, "try-error")) {
		if (variancelog == 0) {
			return(dst_degenerate(exp(meanlog)))
		}
		if (variancelog < 0) {
			stop("'variancelog' parameter must be non-negative.")
		}
	}
	res <- list(parameters = list(
		meanlog = meanlog,
		variancelog = variancelog
	))
	new_parametric(
		res,
		variable = "continuous",
		class    = "lnorm"
	)
}


#' @export
mean.lnorm <- function(x, ...) {
	with(parameters(x), {
		exp(meanlog + variancelog / 2)
	})
}

#' @export
median.lnorm <- function(x, ...) {
	with(parameters(x), {
		exp(meanlog)
	})
}

#' @export
variance.lnorm <- function(x, ...) {
	with(parameters(x), {
		ev <- exp(variancelog)
		(ev - 1) * ev * exp(2 * meanlog)
	})
}


#' @export
skewness.lnorm <- function(x, ...) {
	with(parameters(x), {
		ev <- exp(variancelog)
		(ev + 2) * sqrt(ev - 1)
	})
}

#' @export
kurtosis_exc.lnorm <- function(x, ...) {
	with(parameters(x), {
		e4 <- exp(4 * variancelog)
		e3 <- exp(3 * variancelog)
		e2 <- exp(2 * variancelog)
		e4 + 2 * e3 + 3 * e2 - 6
	})
}

#' @export
eval_cdf.lnorm <- function(object, at) {
	with(parameters(x), {
		stats::plnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
eval_survival.lnorm <- function(object, at) {
	with(parameters(x), {
		stats::plnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog),
					  lower.tail = FALSE)
	})
}

#' @export
eval_density.lnorm <- function(object, at) {
	with(parameters(x), {
		stats::dlnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
realise.lnorm <- function(object, n = 1, ...) {
	with(parameters(x), {
		stats::rlnorm(n, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
eval_quantile.lnorm <- function(object, at, ...) {
	with(parameters(x), {
		stats::qlnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @rdname range
#' @export
range.lnorm <- function(x, ...) {
	c(0, Inf)
}

#' @rdname discontinuities
#' @export
discontinuities.lnorm <- function(object, from = -Inf, to = Inf, ...) {
	if (from > to) {
		stop("'to' argument must be larger or equal than from argument")
	}
	make_empty_discontinuities_df()
}

# Using .dst method for:
# - get_hazard
# - get_chf
