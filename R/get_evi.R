#' Get the EVI of a distribution
#'
#' EVI = Extreme value index
#'
#' @param object Distribution object to obtain EVI from.
#' @details Doesn't calculate EVI if not present in the object.
#' @return A single numeric
#' @export
get_evi <- function(object) UseMethod("get_evi")

#' @export
get_evi.dst <- function(object) object$prop$evi

#' @export
get_evi.stepdst <- function(object) NaN
