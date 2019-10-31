#' Make an Empirical distribution
#'
#' Makes an empirical distribution from a sample of univariate
#' data.
#' @param x Vector of values to construct empirical distribution from.
#' @return Object of class "dst".
#' @export
dst_emp <- function(x) {
	if (length(x) == 1) return(dst_degen(x))
	vals <- na.omit(x)
	mu <- mean(x, na.rm = TRUE)
	ss <- mean((x - mu) ^ 2, na.rm = TRUE)
	sig <- sqrt(ss)
	dst(pdst = stats::ecdf(x),
		qdst = eqf(x),
		pdst = epmf(x),
		rdst = function(n) sample(vals, size = n, replace = TRUE),
		prop = list(mean = mu,
					var  = ss,
					sd   = sig,
					evi  = NA,
					has_pmf = TRUE,
					has_pdf = FALSE))
}
