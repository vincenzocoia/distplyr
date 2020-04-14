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
	get_chf(object)(at)
}

#' @export
get_chf.dst <- function(object) {
	if (variable(object) == "continuous") {
		sf <- get_survival(object)
		function(x) -log(sf(x))
	}
}
