#' @export
eval_cdf.norm <- function(object, at) {
	with(parameters(object), {
		stats::pnorm(at, mean = mean, sd = sqrt(variance))
	})
}

#' @export
eval_survival.norm <- function(object, at) {
	with(parameters(object), {
		stats::pnorm(at, mean = mean, sd = sqrt(variance), lower.tail = FALSE)
	})
}

#' @export
eval_density.norm <- function(object, at, strict = TRUE) {
	with(parameters(object), {
		stats::dnorm(at, mean = mean, sd = sqrt(variance))
	})
}

#' @export
realise.norm <- function(object, n = 1, ...) {
	with(parameters(object), {
		stats::rnorm(n, mean = mean, sd = sqrt(variance))
	})
}

#' @export
eval_quantile.norm <- function(object, at, ...) {
	with(parameters(object), {
		stats::qnorm(at, mean = mean, sd = sqrt(variance))
	})
}
