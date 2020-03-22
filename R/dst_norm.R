#' Normal (Gaussian) Distribution
#'
#' Makes a distribution belonging to the family of
#' Normal (Gaussian) distributions.
#' @param mu,var Mean and variance of the distribution.
#' @return Object of class "dst".
#' @export
dst_norm <- function(mu, var) {
	if (var == 0) return(dst_degen(mu))
	if (var < 0) stop("'var' parameter must be non-negative.")
	sd <- sqrt(var)
	dst(fun_cumu  = function(x) pnorm(x, mean = mu, sd = sd),
		fun_quant = function(p) qnorm(p, mean = mu, sd = sd),
		fun_prob  = function(x) dnorm(p, mean = mu, sd = sd),
		fun_rand  = function(n) rnorm(n, mean = mu, sd = sd),
		fun_surv  = function(x) pnorm(x, mean = mu, sd = sd, lower.tail = FALSE),
		name = "Normal/Gaussian",
		param = c(mu = mu, var = var),
		prop = list(mean = mu,
					var  = var,
					sd   = sd,
					evi  = 0,
					has_pdf = TRUE,
					has_pmf = FALSE))
}
