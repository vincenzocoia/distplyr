#' Get the EVI of a distribution
#'
#' EVI = Extreme value index
#'
#' @param object Distribution object to obtain EVI from.
#' @details Doesn't calculate EVI if not present in the object.
#' @return A single numeric
#' @examples
#' get_evi(dst_gpd(0, 1, 2))
#' get_evi(dst_gpd(0, 1, -1))
#' get_evi(dst_unif(0, 10))
#' get_evi(dst_norm(5, 5))
#' get_evi(stepdst(1:10))
#' @export
get_evi <- function(object) UseMethod("get_evi")

#' @export
get_evi.dst <- function(object) object$prop$evi

#' @export
get_evi.stepdst <- function(object) NaN
