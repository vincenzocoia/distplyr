#' Evaluate probability density/mass function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the density or mass function at
#' @return Vector of density/mass values
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @export
eval_probfn <- function(object, at) UseMethod("eval_probfn")

#' Get probability density/mass function
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the pdf/pmf.
#' @seealso
#' \code{\link{get_quantile}},
#' \code{\link{get_cdf}},
#' \code{\link{get_randfn}},
#' \code{\link{get_hazard}},
#' \code{\link{get_survival}}
#' @export
get_probfn <- function(object) UseMethod("get_probfn")


#' @export
eval_probfn.dst <- function(object, at) {
	get_probfn(object)(at)
}

#' @export
get_probfn.dst <- function(object) object$fun_prob

