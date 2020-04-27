#' Hazard Function
#'
#' Hazard and cumulative hazard functions.
#' @param object Distribution object
#' @rdname hazard
#' @export
get_chf <- function(object) UseMethod("get_chf")

#' @rdname hazard
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
