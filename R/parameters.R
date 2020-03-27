#' @export
parameters <- function(object) UseMethod("parameters")

#' @export
parameters.parametric <- function(object) {
	object[["params"]]
}
