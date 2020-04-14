#' Name of a Distribution
#'
#' @param object A distribution object
#' @export
name <- function(object) UseMethod("name")

#' @export
name.dst <- function(object) {
	object[["name"]]
}
