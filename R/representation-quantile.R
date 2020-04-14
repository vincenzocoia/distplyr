#' Evaluate quantile function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the quantile function at
#' @return Vector of quantiles
#' @seealso
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @export
eval_quantile <- function(object, at) UseMethod("eval_quantile")


#' Get quantile function
#'
#' @param object Object of class "dst"
#' @param ... Other arguments to pass to specific methods
#' @return a vectorized function of the quantile function.
#' @seealso
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_randfn}},
#' \code{\link{get_hazard}},
#' \code{\link{get_survival}}
#' @rdname get_quantile
#' @export
get_quantile <- function(object, ...) UseMethod("get_quantile")

#' @param tol tolerance
#' @param maxiter Maximum number of iterations
#' @rdname get_quantile
#' @export
get_quantile.dst <- function(object, tol = 1e-6, maxiter = 1000, ...) {
	f <- object[["representations"]][["fun_quant"]]
	if (!is.null(f)) return(f)
	function(x) eval_quantile_from_cdf(object, x, tol = tol, maxiter = maxiter)
}

#' @export
eval_quantile.dst <- function(object, at) {
	get_quantile(object)(at)
}
