#' Make an Empirical distribution
#'
#' Makes an empirical distribution from a sample of univariate
#' data.
#' @param x Vector of values to construct empirical distribution from.
#' @return Object of class "dst".
#' @seealso \code{\link{eqf}}, \code{\link{epmf}}
#' @export
dst_emp <- function(x) {
	if (length(x) == 1) return(dst_degen(x))
	vals <- stats::na.omit(x)
	mu <- mean(x, na.rm = TRUE)
	ss <- mean((x - mu) ^ 2, na.rm = TRUE)
	sig <- sqrt(ss)
	dst(pdst = stats::ecdf(x),
		qdst = eqf(x),
		ddst = epmf(x),
		rdst = function(n) sample(vals, size = n, replace = TRUE),
		name = "Empirical",
		param = x,
		prop = list(mean = mu,
					var  = ss,
					sd   = sig,
					evi  = NA,
					has_pmf = TRUE,
					has_pdf = FALSE))
}
