#' Get the EVI of a distribution
#'
#' EVI = Extreme value index
#'
#' @param object Distribution object to obtain EVI from.
#' @details Doesn't calculate EVI if not present in the object.
#' @return A single numeric
#' @examples
#' evi(dst_gpd(0, 1, 2))
#' evi(dst_gpd(0, 1, -1))
#' evi(dst_unif(0, 10))
#' evi(dst_norm(5, 5))
#' evi(stepdst(1:10))
#' @export
evi <- function(object) UseMethod("evi")

#' @export
evi.dst <- function(object) object$prop$evi

#' @export
evi.stepdst <- function(object) NaN
