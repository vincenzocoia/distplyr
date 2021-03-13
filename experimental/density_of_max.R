density_of_max <- function(dst, n, at) {
	cdf <- eval_cdf(dst, at)
	pdf <- eval_density(dst, at)
	res <- n * cdf ^ (n - 1) * pdf
	tibble::tibble(.arg = at, .density = res)
}
