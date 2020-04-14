#' Parameters of a Distribution
#'
#' @param object Distribution object
#' @export
parameters <- function(object) UseMethod("parameters")

#' @export
parameters.dst <- function(object) {
	object[["parameters"]]
}
