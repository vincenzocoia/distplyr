#' Probability density/mass function
#'
#' Access a distribution's probability density or mass function.
#'
#' @inheritParams get_cdf
#' @return A vector of the evaluated pmf/pdf, in the case of \code{eval_}; a
#'   data frame with both the argument and function evaluations, in the case of
#'   \code{enframe_}; or a vectorized function representing the pmf/pdf, in the
#'   case of \code{get_}.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_probfn(d, at = 0:4)
#' enframe_probfn(d, at = 0:4)
#' probfn <- get_probfn(d)
#' probfn(0:4)
#' @family distributional representations
#' @rdname probfn
#' @export
eval_probfn <- function(object, at) UseMethod("eval_probfn")

#' @rdname probfn
#' @export
get_probfn <- function(object) UseMethod("get_probfn")


#' @export
eval_probfn.dst <- function(object, at) {
	f <- object[["representations"]][["fun_probfn"]]
	if (!is.null(f)) return(f(at))
	stop("Can't find a probability function for this distribution.")
}

#' @export
get_probfn.dst <- function(object) {
	f <- object[["representations"]][["fun_probfn"]]
	if (!is.null(f)) return(f)
	function(at) eval_probfn(object, at = at)
}

#' @rdname probfn
#' @export
enframe_probfn <- function(object, at,
							 arg_name = ".arg",
							 fn_name = ".probfn") {
	UseMethod("enframe_probfn")
}

#' @export
enframe_probfn.dst <- function(object, at,
								 arg_name = ".arg",
								 fn_name = ".probfn") {
	f <- eval_probfn(object, at = at)
	res <- data.frame(at, f)
	names(res) <- c(arg_name, fn_name)
	res
}
