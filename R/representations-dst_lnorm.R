#' @export
eval_cdf.lnorm <- function(distribution, at) {
	with(parameters(distribution), {
		stats::plnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
eval_survival.lnorm <- function(distribution, at) {
	with(parameters(distribution), {
		stats::plnorm(at,
					  meanlog = meanlog, sdlog = sqrt(variancelog),
					  lower.tail = FALSE
		)
	})
}

#' @export
eval_density.lnorm <- function(distribution, at, strict = TRUE) {
	with(parameters(distribution), {
		stats::dlnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
eval_quantile.lnorm <- function(distribution, at, ...) {
	with(parameters(distribution), {
		stats::qlnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
realise.lnorm <- function(distribution, n = 1, ...) {
	with(parameters(distribution), {
		stats::rlnorm(n, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}
