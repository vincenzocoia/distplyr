#' Get the EVI of a distribution
#'
#' Extreme value index
#'
#' @param object Object of class "dst" to obtain mean from.
#' @details Doesn't calculate if not present.
#' @return A single numeric
evi.dst <- function(object) object$prop$evi

#' @export
evi <- function(...) UseMethod("evi")
