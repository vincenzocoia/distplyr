#' @export
eval_cdf.lnorm <- function(object, at) {
	with(parameters(object), {
		stats::plnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
eval_survival.lnorm <- function(object, at) {
	with(parameters(object), {
		stats::plnorm(at,
					  meanlog = meanlog, sdlog = sqrt(variancelog),
					  lower.tail = FALSE
		)
	})
}

#' @export
eval_density.lnorm <- function(object, at, strict = TRUE) {
	with(parameters(object), {
		stats::dlnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
eval_quantile.lnorm <- function(object, at, ...) {
	with(parameters(object), {
		stats::qlnorm(at, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}

#' @export
realise.lnorm <- function(object, n = 1, ...) {
	with(parameters(object), {
		stats::rlnorm(n, meanlog = meanlog, sdlog = sqrt(variancelog))
	})
}
