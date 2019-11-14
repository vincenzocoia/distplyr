#' Make a Uniform distribution
#'
#' Makes a distribution belonging to the continuous uniform family of
#' distributions.
#' @param min,max Parameters of the distribution family.
#' @return Object of class "dst".
#' @export
dst_unif <- function(min = 0, max = 1) {
	if (max < min) stop("Parameter 'min' must be less than 'max'.")
	if (max == min) return(dst_degen(min))
	ss <- (max - min) ^ 2 / 12
	sig <- sqrt(ss)
	dst(fun_cumu = function(x) stats::punif(x, min = min, max = max),
		fun_quant = function(x) stats::qunif(x, min = min, max = max),
		fun_prob = function(x) stats::dunif(x, min = min, max = max),
		fun_rand = function(x) stats::runif(x, min = min, max = max),
		name = "Uniform",
		param = c(min = min, max = max),
		prop = list(mean = (min + max) / 2,
					var  = ss,
					sd   = sig,
					evi  = -1,
					has_pdf = TRUE,
					has_pmf = FALSE)
		)
}
