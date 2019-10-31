#' Make a Degenerate Distribution
#'
#' Makes a distribution belonging to the degenerate family of
#' distributions. That is, distributions of fixed values.
#' @param loc Parameter of the distribution family.
#' @return Object of class "dst".
#' @export
dst_degen <- function(loc) {
	dst(pdst = stats::ecdf(loc),
		qdst = eqf(loc),
		ddst = function(x) as.numeric(x == loc),
		rdst = function(n) rep(loc, n),
		name = "Degenerate",
		param = c(loc = loc),
		prop = list(mean = loc,
					var  = 0,
					sd   = 0,
					evi  = NA,
					has_pdf = FALSE,
					has_pmf = TRUE))
}
