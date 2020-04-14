#' Evaluate cdf
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the cdf at
#' @return Vector of cdf values
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @export
eval_cdf <- function(object, at) UseMethod("eval_cdf")


#' Get cdf
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the cdf
#' @seealso
#' \code{\link{get_quantile}},
#' \code{\link{get_probfn}},
#' \code{\link{get_randfn}},
#' \code{\link{get_hazard}},
#' \code{\link{get_survival}}
#' @export
get_cdf <- function(object) UseMethod("get_cdf")


#' @export
eval_cdf.dst <- function(object, at) {
	get_cdf(object)(at)
}

#' @export
get_cdf.dst <- function(object) object$fun_cumu

