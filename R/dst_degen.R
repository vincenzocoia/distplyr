#' Make a Degenerate Distribution
#'
#' Makes a distribution belonging to the degenerate family of
#' distributions. That is, distributions of fixed values.
#' @param loc Parameter of the distribution family.
#' @return Object of class "dst".
#' @export
dst_degen <- function(loc) {
	dst(fun_cumu = stats::ecdf(loc),
		fun_quant = eqf(loc),
		fun_prob = function(x) as.numeric(x == loc),
		fun_rand = function(n) rep(loc, n),
		name = "Degenerate",
		param = c(loc = loc),
		prop = list(mean = loc,
					var  = 0,
					sd   = 0,
					evi  = NA,
					has_pdf = FALSE,
					has_pmf = TRUE))
}
