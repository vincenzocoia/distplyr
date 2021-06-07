#' Random Variable of a Distribution
#'
#' Information about a random variable of a distribution.
#'
#' @param object Distribution object
#' @return As of now, the type of variable that it is -- one
#' of "continuous", "discrete", or "mixed".
#' @rdname variable
#' @export
variable <- function(object) {
  attributes(object)[["variable"]]
}
