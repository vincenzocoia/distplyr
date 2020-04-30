#' Probability Mass Function
#'
#' Access a distribution's probability mass function (pmf).
#'
#' @inheritParams get_cdf
#' @return A vector of the evaluated pmf/pdf, in the case of \code{eval_}; a
#'   data frame with both the argument and function evaluations, in the case of
#'   \code{enframe_}; or a vectorized function representing the pmf/pdf, in the
#'   case of \code{get_}.
#' @examples
#' d <- stepdst(1:10)
#' eval_pmf(d, at = c(1, 2, 2.5))
#' enframe_pmf(d, at = 0:4)
#' pmf <- get_pmf(d)
#' pmf(0:4)
#' @family distributional representations
#' @rdname pmf
#' @export
eval_pmf <- function(object, at) UseMethod("eval_pmf")

#' @rdname pmf
#' @export
get_pmf <- function(object) UseMethod("get_pmf")


#' @export
eval_pmf.dst <- function(object, at) {
	f <- object[["representations"]][["fun_pmf"]]
	if (!is.null(f)) return(f(at))
	if (variable(object) != "discrete") return(NULL)
	stop("Can't find a probability function for this distribution.")
}

#' @export
get_pmf.dst <- function(object) {
	f <- object[["representations"]][["fun_pmf"]]
	if (!is.null(f)) return(f)
	if (variable(object) != "discrete") return(NULL)
	function(at) eval_pmf(object, at = at)
}

#' @rdname pmf
#' @export
enframe_pmf <- function(object, at,
						   arg_name = ".arg",
						   fn_name = ".pmf") {
	UseMethod("enframe_pmf")
}

#' @export
enframe_pmf.dst <- function(object, at,
							   arg_name = ".arg",
							   fn_name = ".pmf") {
	f <- eval_pmf(object, at = at)
	if (is.null(f)) return(NULL)
	res <- data.frame(at, f)
	names(res) <- c(arg_name, fn_name)
	res
}
