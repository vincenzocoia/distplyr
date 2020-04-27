#' Evaluate Hazard Function
#'
#' Evaluate hazard function or cumulative
#' hazard function (chf).
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the hazard function at.
#' @return Vector of the evaluated hazard function
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{realise}},
#' \code{\link{eval_survival}}
#' @rdname hazard
#' @export
eval_hazard <- function(object, at) UseMethod("eval_hazard")


#' @rdname hazard
#' @export
get_hazard <- function(object) UseMethod("get_hazard")

#' @export
eval_hazard.dst <- function(object, at) {
	if (identical(variable(object), "continuous")) {
		sf <- eval_survival(object, at)
		pdf <- eval_probfn(object, at)
		pdf / sf
	} else {
		stop("Not programmed yet.")
	}
}

#' @export
get_hazard.dst <- function(object) {
	hf <- object[["representations"]][["fun_hazard"]]
	if (!is.null(hf)) return(hf)
	function(at) eval_hazard(object, at = at)
}
