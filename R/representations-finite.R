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
eval_pmf.finite <- function(object, at, strict = TRUE) {
	with(object$probabilities, {
		vapply(at, function(x) sum(size[x == location]), FUN.VALUE = numeric(1L))
	})
}

#' @export
realise.finite <- function(object, n = 1, ...) {
	with(object$probabilities, {
		sample(location, size = n, replace = TRUE, prob = size)
	})
}
