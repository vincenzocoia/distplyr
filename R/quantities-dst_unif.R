#' @export
mean.unif <- function(x, ...) {
	with(parameters(x), {
		res <- resolve_if_possible((!!min + !!max) / 2)
		if (res$resolved) {
			res$outcome
		} else {
			rlang::expr_print(res$outcome)
		}
	})
}

#' @export
median.unif <- function(x, ...) {
	with(parameters(x), (min + max) / 2)
}

#' @export
variance.unif <- function(x, ...) {
	with(parameters(x), (min - max)^2 / 12)
}


#' @export
evi.unif <- function(x, ...) {
	-1
}

#' @export
skewness.unif <- function(x, ...) {
	0
}

#' @export
kurtosis_exc.unif <- function(x, ...) {
	-6 / 5
}

#' @rdname range
#' @export
range.unif <- function(x, ...) {
	with(parameters(x), {
		c(min, max)
	})
}
