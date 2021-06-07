# #' Specify a Distribution Manually
# #'
# #' Make a distribution object.
# #' @param fun_cumu,fun_quant cdf and quantile function for a distribution,
# #' respectively. Must be supplied.
# #' @param fun_prob,fun_rand Probability mass/density function and
# #' random number generator for a distribution (a function of n), respectively.
# #' Optional. \code{fun_rand} will be calculated from \code{fun_quant}
# #' if missing.
# #' @param fun_surv Survival function. Optional; is missing, will be
# #' derived from the cdf.
# #' @param prop Properties of the distribution, such as mean, variance,
# #' EVI, etc. (of your choosing).
# #' @param variable Type of random variable: "continuous", "discrete",
# #' or "mixed".
# #' @param name A name for the distribution (such as a parametric family name)
# #' @param param Parameters for the distribution, if parameteric.
# #' @return An object of class "dst", which (for now) is a list holding
# #' the above arguments, including survival and hazard functions.
# #' @rdname dst
# #' @export
# dst_manual <- function(fun_cumu, fun_quant, fun_prob, fun_rand, fun_surv,
# 				variable = c("continuous", "discrete", "mixed"),
# 				name = NULL, param = NULL, prop = NULL) {
# 	if (missing(fun_surv)) {
# 		if (!missing(fun_cumu)) {
# 			fun_surv <- function(x) 1 - fun_cumu(x)
# 		} else {
# 			fun_surv <- NULL
# 		}
# 	}
# 	if (missing(fun_prob)) {
# 		fun_prob <- NULL
# 		fun_haz <- NULL
# 	} else {
# 		fun_haz <- function(x) fun_prob(x) / (1 - fun_cumu(x))
# 	}
# 	if (missing(fun_rand)) {
# 		fun_rand <- function(n) fun_quant(stats::runif(n))
# 	}
# 	x <- list(representations = list(fun_cumu = fun_cumu,
# 									 fun_quant = fun_quant,
# 									 fun_prob = fun_prob,
# 									 fun_rand = fun_rand,
# 									 fun_surv = fun_surv,
# 									 fun_haz = fun_haz),
# 			  name = name,
# 			  parameters = param,
# 			  prop = prop)
# 	v <- match.arg(variable)
# 	new_dst(x, variable = v)
# }
