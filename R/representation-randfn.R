#' Random sampler function from a distribution
#'
#' @param object Object of class "dst"
#' @return a function accepting an integer number of values to generate.
#' @seealso
#' \code{\link{get_quantile}},
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_hazard}},
#' \code{\link{get_survival}}
#' @export
get_randfn <- function(object) UseMethod("get_randfn")

#' Generate random sample from a distribution
#'
#' @param object Object of class "dst"
#' @param at Number of observations to generate
#' @return Vector of independent values drawn from the inputted distribution
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @export
eval_randfn <- function(object, at) UseMethod("eval_randfn")

#' @export
eval_randfn.dst <- function(object, at) {
	get_randfn(object)(at)
}

#' @export
get_randfn.dst <- function(object) {
	r <- object[["representations"]][["fun_rand"]]
	if (is.null(r)) {
		qf <- get_quantile(object)
		function(n) qf(stats::runif(n))
	} else {
		r
	}
}
