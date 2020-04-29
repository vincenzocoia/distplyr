#' Distribution Quantiles
#'
#' @param object Object of class "dst"
#' @param at Vector of quantile levels (probabilities).
#' @param ... Other arguments to pass to specific methods
#' @return Vector of quantiles
#' @seealso
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @rdname quantile
#' @export
eval_quantile <- function(object, at, ...) UseMethod("eval_quantile")

#' @rdname quantile
#' @export
get_quantile <- function(object, ...) UseMethod("get_quantile")

#' @param tol tolerance
#' @param maxiter Maximum number of iterations
#' @rdname quantile
#' @export
get_quantile.dst <- function(object, tol = 1e-6, maxiter = 1000, ...) {
	f <- object[["representations"]][["fun_quant"]]
	if (!is.null(f)) return(f)
	function(at) eval_quantile(object, at, tol = tol, maxiter = maxiter, ...)
}

#' @rdname quantile
#' @export
eval_quantile.dst <- function(object, at, tol = 1e-6, maxiter = 1000, ...) {
	f <- object[["representations"]][["fun_quant"]]
	if (!is.null(f)) return(f(at))
	cdf <- get_cdf(object)
	eval_quantile_from_cdf(cdf, at, tol = tol, maxiter = maxiter)
}

#' @rdname quantile
#' @export
enframe_quantile <- function(object, at,
							 arg_name = ".arg",
							 fn_name = ".quantile") {
	UseMethod("enframe_quantile")
}

#' @export
enframe_quantile.dst <- function(object, at,
								 arg_name = ".arg",
								 fn_name = ".quantile") {
	f <- eval_quantile(object, at = at)
	res <- data.frame(at, f)
	names(res) <- c(arg_name, fn_name)
	res
}
