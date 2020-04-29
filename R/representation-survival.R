#' Evaluate Survival Function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the survival function at.
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_hazard}},
#' @rdname survival
#' @export
eval_survival <- function(object, at) UseMethod("eval_survival")

#' @rdname survival
#' @export
get_survival <- function(object) UseMethod("get_survival")


#' @export
get_survival.dst <- function(object) {
	sf <- object[["representations"]][["fun_survival"]]
	if (!is.null(sf)) return(sf)
	function(at) eval_survival(object, at = at)
}


#' @export
eval_survival.dst <- function(object, at) {
	sf <- object[["representations"]][["fun_survival"]]
	if (is.null(sf)) {
		cdf <- get_cdf(object)
		1 - cdf(at)
	} else {
		sf(at)
	}
}

#' @rdname survival
#' @export
enframe_survival <- function(object, at,
							 arg_name = ".arg",
							 fn_name = ".survival") {
	UseMethod("enframe_survival")
}

#' @export
enframe_survival.dst <- function(object, at,
								 arg_name = ".arg",
								 fn_name = ".survival") {
	f <- eval_survival(object, at = at)
	res <- data.frame(at, f)
	names(res) <- c(arg_name, fn_name)
	res
}
