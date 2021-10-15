#' @export
eval_cdf.norm <- function(distribution, at) {
	with(parameters(distribution), {
		stats::pnorm(at, mean = mean, sd = sqrt(variance))
	})
}

#' @export
eval_survival.norm <- function(distribution, at) {
	with(parameters(distribution), {
		stats::pnorm(at, mean = mean, sd = sqrt(variance), lower.tail = FALSE)
	})
}

#' @export
eval_density.norm <- function(distribution, at, strict = TRUE) {
	with(parameters(distribution), {
		stats::dnorm(at, mean = mean, sd = sqrt(variance))
	})
}

#' @export
realise.norm <- function(distribution, n = 1, ...) {
	with(parameters(distribution), {
		stats::rnorm(n, mean = mean, sd = sqrt(variance))
	})
}

#' @export
eval_quantile.norm <- function(distribution, at, ...) {
	with(parameters(distribution), {
		stats::qnorm(at, mean = mean, sd = sqrt(variance))
	})
}
