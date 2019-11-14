#' Make a distribution
#'
#' Make a distribution object.
#' @param fun_cumu,fun_quant,fun_prob,fun_rand cdf, quantile function, density/mass
#' function, and random number generator for a distribution.
#' @param prop Properties of the distribution, such as mean, variance,
#' EVI, etc. (of your choosing).
#' @param name A name for the distribution (such as a parametric family name)
#' @param param Parameters for the distribution, if parameteric.
#' @return An object of class "dst", which (for now) is a list holding
#' the above arguments, including survival and hazard functions.
#' @export
dst <- function(fun_cumu, fun_quant, fun_prob, fun_rand,
				name = NULL, param = NULL, prop = NULL) {
	fun_surv <- function(x) 1 - fun_cumu(x)
	if (missing(fun_prob)) {
		fun_haz <- NULL
	} else {
		fun_haz <- function(x) fun_prob(x) / (1 - fun_cumu(x))
	}
	x <- list(fun_cumu = fun_cumu,
			  fun_quant = fun_quant,
			  fun_prob = fun_prob,
			  fun_rand = fun_rand,
			  fun_surv = fun_surv,
			  fun_haz = fun_haz,
			  name = name,
			  param = param,
			  prop = prop)
	class(x) <- "dst"
	x
}
