#' Set a representation of a distribution
#'
#' Manually specify a representation of a distribution.
#'
#' @param object Distribution object.
#' @param fn Vectorized function of the representation.
#' @return A distribution object, as given in \code{object},
#' containing the specified representation.
#' @rdname set_representation
#' @export
set_cdf <- function(object, fn) UseMethod("set_cdf")

#' @rdname set_representation
#' @export
set_survival <- function(object, fn) UseMethod("set_survival")

#' @rdname set_representation
#' @export
set_quantile <- function(object, fn) UseMethod("set_quantile")

#' @rdname set_representation
#' @export
set_density <- function(object, fn) UseMethod("set_density")

#' @rdname set_representation
#' @export
set_pmf <- function(object, fn) UseMethod("set_pmf")

#' @rdname set_representation
#' @export
set_hazard <- function(object, fn) UseMethod("set_hazard")

#' @rdname set_representation
#' @export
set_chf <- function(object, fn) UseMethod("set_chf")


#' @export
set_cdf.dst <- function(object, fn) {
  object[["representations"]][["cdf"]] <- fn
  object
}

#' @export
set_density.dst <- function(object, fn) {
  object[["representations"]][["density"]] <- fn
  object
}

#' @export
set_quantile.dst <- function(object, fn) {
  object[["representations"]][["quantile"]] <- fn
  object
}

#' @export
set_pmf.dst <- function(object, fn) {
  object[["representations"]][["pmf"]] <- fn
  object
}

#' @export
set_hazard.dst <- function(object, fn) {
  object[["representations"]][["hazard"]] <- fn
  object
}

#' @export
set_chf.dst <- function(object, fn) {
  object[["representations"]][["chf"]] <- fn
  object
}

#' @export
set_survival.dst <- function(object, fn) {
  object[["representations"]][["survival"]] <- fn
  object
}
