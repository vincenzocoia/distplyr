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

#' @export
#' @inheritParams next_discrete
next_discrete.finite <- function(object, from, n = 1L, ...) {
  all_discretes <- object$probabilities$location
  higher_discretes <- all_discretes[all_discretes > from]
  sort(higher_discretes)[seq_along(n)]
}

#' @export
#' @inheritParams next_discrete
next_discrete.degenerate <- function(object, from, n = 1L, ...) {
  with(parameters(object), {
    if (from >= location) {
      NA_real_
    } else {
      res <- numeric(n)
      if (n == 0) {
        return(numeric(0L))
      } else {
        c(location, rep(NA_real_, n - 1L))
      }
    }
  })
}
