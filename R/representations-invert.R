#' @export
eval_cdf.inverse <- function(distribution, at) {
	dist <- distribution$distribution
	distionary::eval_cdf(dist, 0) -
		distionary::eval_cdf(dist, 1 / at) +
		distionary::eval_pmf(dist, 1 / at) +
		as.numeric(at >= 0)
}

#' @export
eval_density.inverse <- function(distribution, at) {
	distionary::eval_density(distribution$distribution, 1 / at) / at^2
}

#' @export
eval_pmf.inverse <- function(distribution, at) {
	distionary::eval_pmf(distribution$distribution, 1 / at)
}

#' @export
eval_quantile.inverse <- function(distribution, at) {
	quantile_0 <- distionary::eval_quantile(distribution$distribution, at = 0)
	1 / distionary::eval_quantile(d, quantile_0 + (at > quantile_0) - at)
}

#' @export
realise.inverse <- function(distribution, at) {
	1 / distionary::realise(distribution$distribution, at)
}
