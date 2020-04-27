#' Probability density/mass function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the density or mass function at
#' @return Vector of density/mass values
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_cdf}},
#' \code{\link{realise}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @rdname probfn
#' @export
eval_probfn <- function(object, at) UseMethod("eval_probfn")

#' @rdname probfn
#' @export
get_probfn <- function(object) UseMethod("get_probfn")


#' @export
eval_probfn.dst <- function(object, at) {
	get_probfn(object)(at)
}

#' @export
get_probfn.dst <- function(object) object$fun_prob

