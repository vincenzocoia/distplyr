#' Transform a Distribution
#'
#' If \code{X} has distribution \code{.dst}, then this function
#' returns the distribution of \code{g(X)}
#'
#' @param dist Original distribution to transform
#' @param g A (strictly increasing or decreasing) function, vectorized.
#' @param ginv Inverse function of \code{g}, vectorized.
#' @param gprime,ginvprime Derivative functions of \code{g} and \code{ginv},
#' vectorized.
#' Not required if there is no density, and if there is a density, only
#' one is required.
#' @return Object of class "dst" of the transformed random variable.
#' @export
rv_transform <- function(.dst, g, ginv, gprime, ginvprime) {
	if (missing(ginvprime) && !missing(gprime)) {
		ginvprime <- function(x) 1 / gprime(ginv(x))
	}
	.pdst <- pdst(.dst)
	.qdst <- qdst(.dst)
	.rdst <- rdst(.dst)
	.ddst <- ddst(.dst)
	.has_pdf <- has_pdf(.dst)
	.has_pmf <- has_pmf(.dst)
	if (.has_pdf) {
		if (missing(ginvprime)) {
			warning("Didn't provide a derivative of the transformation, ",
					"yet .dst has a density. New distribution will be ",
					"missing this density.")
			new_ddst <- NA
		} else {
			new_ddst <- function(x) .ddst(ginv(x)) * ginvprime(x)
		}
	} else if (.has_pmf) {
		new_ddst <- function(x) .ddst(ginv(x))
	} else {
		new_ddst <- NA
	}
	dst(name = paste("Transformed", .dst$name),
		param = NULL,
		pdst = function(x) .pdst(ginv(x)),
		qdst = function(x) g(.qdst(x)),
		rdst = function(n) g(.rdst(n)),
		ddst = new_ddst,
		prop = list(mean = NA,
					sd   = NA,
					var  = NA,
					evi  = NA,
					has_pdf = .has_pdf,
					has_pmf = .has_pmf))
}
