#' Kurtosis
#'
#' Get the kurtosis (raw) or excess kurtosis (exc) of a distribution.
#'
#' @param object Distribution object
#' @rdname kurtosis
#' @export
get_kurtosis_raw <- function(object) UseMethod("get_kurtosis_raw")

#' @rdname kurtosis
#' @export
get_kurtosis_exc <- function(object) UseMethod("get_kurtosis_exc")

#' @export
get_kurtosis_raw.dst <- function(object) {
	3 + get_kurtosis_exc(object)
}
