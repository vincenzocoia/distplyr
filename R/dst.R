#' Make a distribution
#'
#' Make a distribution object.
#' @param pdst,qdst,ddst,rdst cdf, quantile function, density/mass
#' function, and random number generator for a distribution.
#' @param prop Properties of the distribution, such as mean, variance,
#' EVI, etc. (of your choosing).
#' @param name A name for the distribution (such as a parametric family name)
#' @param param Parameters for the distribution, if parameteric.
#' @return An object of class "dst", which (for now) is a list holding
#' the above arguments.
#' @export
dst <- function(pdst, qdst, ddst, rdst,
				name = NULL, param = NULL, prop = NULL) {
	x <- list(pdst = pdst,
			  qdst = qdst,
			  ddst = ddst,
			  rdst = rdst,
			  name = name,
			  param = param,
			  prop = prop)
	class(x) <- "dst"
	x
}
