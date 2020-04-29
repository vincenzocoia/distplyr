#' Cumulative Hazard Function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the cumulative hazard at.
#' @return Vector of the evaluated hazard function
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_survival}}
#' @rdname chf
#' @export
get_chf <- function(object) UseMethod("get_chf")

#' @rdname chf
#' @export
eval_chf <- function(object, at) UseMethod("eval_chf")

#' @export
eval_chf.dst <- function(object, at) {
	if (identical(variable(object), "continuous")) {
		sf <- eval_survival(object, at)
		-log(sf(at))
	} else {
		stop("Not programmed yet")
	}
}

#' @export
get_chf.dst <- function(object) {
	chf <- object[["representations"]][["fun_chf"]]
	if (!is.null(chf)) return(chf)
	function(at) eval_chf(object, at = at)
}

#' @rdname chf
#' @export
enframe_chf <- function(object, at,
							 arg_name = ".arg",
							 fn_name = ".chf") {
	UseMethod("enframe_chf")
}

#' @export
enframe_chf.dst <- function(object, at,
								 arg_name = ".arg",
								 fn_name = ".chf") {
	f <- eval_chf(object, at = at)
	res <- data.frame(at, f)
	names(res) <- c(arg_name, fn_name)
	res
}
