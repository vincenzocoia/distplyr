#' Probability density/mass function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the density or mass function at
#' @return Vector of density/mass values
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_cdf}},
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
