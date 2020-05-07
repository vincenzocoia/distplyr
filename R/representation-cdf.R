#' (Cumulative) Distribution Function
#'
#' Access a distribution's cumulative distribution function (cdf).
#'
#' @param object Distribution object.
#' @param at Vector of values to evaluate the cdf at.
#' @param arg_name Name of the column containing the function arguments, in the
#'   output data frame of \code{enframe_}.
#' @param fn_name Name of the column containing the function evaluations, in the
#'   output data frame of \code{enframe_}.
#' @return A vector of the evaluated cdf, in the case of \code{eval_}; a data
#'   frame with both the argument and function evaluations, in the case of
#'   \code{enframe_}; or a vectorized function representing the cdf, in the case
#'   of \code{get_}.
#' @family distributional representations
#' @examples
#' d <- dst_unif(0, 4)
#' eval_cdf(d, at = 0:4)
#' enframe_cdf(d, at = 0:4)
#' cdf <- get_cdf(d)
#' cdf(0:4)
#' @rdname cdf
#' @export
eval_cdf <- function(object, at) UseMethod("eval_cdf")


#' @rdname cdf
#' @export
get_cdf <- function(object) UseMethod("get_cdf")


#' @export
eval_cdf.dst <- function(object, at) {
	cdf <- object[["representations"]][["cdf"]]
	if (!is.null(cdf)) return(cdf(at))
	stop("Can't find a cdf for this distribution.")
}

#' @export
get_cdf.dst <- function(object) {
	cdf <- object[["representations"]][["cdf"]]
	if (!is.null(cdf)) return(cdf)
	function(at) eval_cdf(object, at = at)
}

#' @rdname cdf
#' @export
enframe_cdf <- function(object, at,
						arg_name = ".arg",
						fn_name = ".cdf") {
	UseMethod("enframe_cdf")
}

#' @export
enframe_cdf.dst <- function(object, at,
							arg_name = ".arg",
							fn_name = ".cdf") {
	f <- eval_cdf(object, at = at)
	res <- data.frame(at, f)
	names(res) <- c(arg_name, fn_name)
	res
}


