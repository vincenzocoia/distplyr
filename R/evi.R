#' Get the EVI of a distribution
#'
#' EVI = Extreme value index
#'
#' @param object Object of class "dst" to obtain EVI from.
#' @details Doesn't calculate EVI if not present in the object.
#' @return A single numeric
#' @export
evi <- function(object) UseMethod("evi")

#' @export
evi.dst <- function(object) object$prop$evi
