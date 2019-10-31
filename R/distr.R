#' Make a distribution
#'
#' Make a distribution object.
#' @param pdst,qdst,ddst,rdst cdf, quantile function, density/mass
#' function, and random number generator for a distribution.
#' @param prop Properties of the distribution, such as mean, variance,
#' EVI, etc. (of your choosing).
#' @return An object of class "dst", which (for now) is a list holding
#' the above arguments.
#' @export
dst <- function(pdst, qdst, ddst, rdst, prop = NULL) {
	x <- list(pdst = pdst,
			  qdst = qdst,
			  ddst = ddst,
			  rdst = rdst,
			  prop = prop)
	class(x) <- "dst"
	x
}
