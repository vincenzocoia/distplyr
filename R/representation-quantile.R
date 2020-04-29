#' Distribution Quantiles
#'
#' Access a distribution's quantiles.
#'
#' @inheritParams get_cdf
#' @param ... Other arguments to pass to specific methods.
#' @return A vector of the evaluated quantiles, in the case of
#' \code{eval_}; a data frame with both the argument and
#' function evaluations, in the case of \code{enframe_};
#' or a vectorized function representing the quantile function, in the
#' case of \code{get_}.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_quantile(d, at = 0:4)
#' enframe_quantile(d, at = 0:4)
#' qf <- get_quantile(d)
#' qf(0:4)
#' @family distributional representations
#' @rdname quantile
#' @export
eval_quantile <- function(object, at, ...) UseMethod("eval_quantile")

#' @rdname quantile
#' @export
get_quantile <- function(object, ...) UseMethod("get_quantile")

#' @param tol Error tolerance when using an algorithm to find the left-inverse
#'   of the cdf.
#' @param maxiter Maximum number of iterations when using an algorithm to find
#'   the left-inverse of the cdf.
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

#' @rdname quantile
#' @export
enframe_quantile.dst <- function(object, at, tol = 1e-6, maxiter = 1000,
								 arg_name = ".arg",
								 fn_name = ".quantile") {
	f <- eval_quantile(object, at = at)
	res <- data.frame(at, f)
	names(res) <- c(arg_name, fn_name)
	res
}
