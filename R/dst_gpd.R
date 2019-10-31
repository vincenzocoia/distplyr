#' Make a generalized Pareto distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param loc,scale,shape Parameters of the GPD.
#' @return Object of class "dst" of a GPD.
#' @export
dst_gpd <- function(loc, scale, shape) {
	if (scale == 0) return(dst_degen(loc))
	if (scale < 0) stop("'scale' parameter must be non-negative.")
	mu <- if_else(shape < 1,
					loc + scale / (1 - shape),
					Inf)
	ss <- if_else(shape < 1/2,
				   scale ^ 2 / (1 - shape) ^ 2 / (1 - 2 * shape),
				   Inf)
	sig <- sqrt(ss)
	dst(pdst = function(x) evd::pgpd(x, loc = loc, scale = scale, shape = shape),
		qdst = function(x) evd::qgpd(x, loc = loc, scale = scale, shape = shape),
		ddst = function(x) evd::dgpd(x, loc = loc, scale = scale, shape = shape),
		rdst = function(x) evd::rgpd(x, loc = loc, scale = scale, shape = shape),
		prop = list(mean = mu,
					var  = ss,
					sd   = sig,
					evi  = shape,
					has_pdf = TRUE,
					has_pmf = FALSE))
}
