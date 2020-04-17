#' Name of a Distribution
#'
#' @param object A distribution object
#' @examples
#' name(dst_unif(0, 1))
#' name(stepdst(3:5))
#' @export
name <- function(object) UseMethod("name")

#' @export
name.dst <- function(object) {
	object[["name"]]
}
