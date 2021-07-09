#' Return neighbouring discrete values from a reference
#'
#' Finds the next `n` largest/smallest discrete value in a distribution
#' from some reference point.
#'
#' @param object Distribution
#' @param from Reference value.
#' @param n Number of discrete values to find.
#' @return Vector containing the `n` discrete points of non-zero
#' probability using `from` as the reference point.
#' @examples
#' nest_discrete(dst_pois(1), from = 1.3)
#' nest_discrete(dst_norm(0, 1), from = 1.3, n = 4)
#' @export
next_discrete <- function(object, from, n = 1L, ...) UseMethod("next_discrete")


#' @export
#' @inheritParams next_discrete
next_discrete.dst <- function(object, from, n = 1L, ...) {
  if (variable(object) == "continuous") {
    rep(NA_real_, n)
  }
  error("Cannot find the next discrete value for this distribution.")
}

#' @export
#' @inheritParams next_discrete
next_discrete.pois <- function(object, from, n = 1L, ...) {
  next_discrete_natural(from, n = n)
}

#' @export
#' @inheritParams next_discrete
next_discrete.finite <- function(object, from, n = 1L, ...) {
  all_discretes <- object$probabilities$location
  next_discrete_finite(all_discretes, from = from, n = n)
}

#' @export
#' @inheritParams next_discrete
next_discrete.degenerate <- function(object, from, n = 1L, ...) {
  x <- parameters(object)[["location"]]
  next_discrete_finite(x, from = from, n = n)
}

#' Next discrete value from a finite set of possibilities
#'
#' @param x A vector of all discrete values.
#' @inheritParams next_discrete
next_discrete_finite <- function(x, from, n = 1L) {
  higher_discretes <- x[x > from]
  sort(higher_discretes)[seq_along(n)]
}

#' Next/Previous discrete natural number
#'
#' Calculates the next/previous natural number (including 0) to
#' the right/left of `from`.
#' @inheritParams next_discrete
next_discrete_natural <- function(from, n = 1L) {
  if (from < 0) {
    return(seq_len(n) - 1L)
  }
  floor(from) + seq_len(n)
}
