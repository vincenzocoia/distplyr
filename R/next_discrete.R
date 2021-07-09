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
#' next_discrete(dst_pois(1), from = 1.3)
#' prev_discrete(dst_pois(1), from = 3, n = 10)
#' next_discrete(dst_norm(0, 1), from = 1.3, n = 4)
#' @export
next_discrete <- function(object, from, n = 1L, ...) UseMethod("next_discrete")

#' @export
prev_discrete <- function(object, from, n = 1L, ...) UseMethod("prev_discrete")

#' @export
#' @inheritParams next_discrete
next_discrete.dst <- function(object, from, n = 1L, ...) {
  if (variable(object) == "continuous") {
    rep(NA_real_, n)
  }
  error("Cannot find the next discrete values for this distribution.")
}

#' @export
#' @inheritParams next_discrete
prev_discrete.dst <- function(object, from, n = 1L, ...) {
  if (variable(object) == "continuous") {
    rep(NA_real_, n)
  }
  error("Cannot find the previous discrete values for this distribution.")
}

#' @export
#' @inheritParams next_discrete
next_discrete.pois <- function(object, from, n = 1L, ...) {
  next_discrete_natural(from, n = n)
}

#' @export
#' @inheritParams next_discrete
prev_discrete.pois <- function(object, from, n = 1L, ...) {
  prev_discrete_natural(from, n = n)
}

#' @export
#' @inheritParams next_discrete
next_discrete.finite <- function(object, from, n = 1L, ...) {
  all_discretes <- object$probabilities$location
  next_discrete_finite(all_discretes, from = from, n = n)
}

#' @export
#' @inheritParams next_discrete
prev_discrete.finite <- function(object, from, n = 1L, ...) {
  all_discretes <- object$probabilities$location
  prev_discrete_finite(all_discretes, from = from, n = n)
}

#' @export
#' @inheritParams next_discrete
next_discrete.degenerate <- function(object, from, n = 1L, ...) {
  x <- parameters(object)[["location"]]
  next_discrete_finite(x, from = from, n = n)
}

#' @export
#' @inheritParams next_discrete
prev_discrete.degenerate <- function(object, from, n = 1L, ...) {
  x <- parameters(object)[["location"]]
  prev_discrete_finite(x, from = from, n = n)
}

#' Helper functions for finding discrete values
#'
#' `*_discrete_finite` finds discrete values from a
#' finite set of possibilities. `*_discret_natural` finds
#' discrete values amongst the natural numbers (including 0).
#'
#' @param x A vector of all possible discrete values.
#' @inheritParams next_discrete
#' @rdname discrete_helpers
next_discrete_finite <- function(x, from, n = 1L) {
  higher_discretes <- x[x > from]
  sort(higher_discretes)[seq_len(n)]
}

#' @rdname discrete_helpers
prev_discrete_finite <- function(x, from, n = 1L) {
  lower_discretes <- x[x < from]
  sort(lower_discretes, decreasing = TRUE)[seq_len(n)]
}

#' @rdname discrete_helpers
next_discrete_natural <- function(from, n = 1L) {
  if (from < 0) {
    return(seq_len(n) - 1L)
  }
  floor(from) + seq_len(n)
}

#' @rdname discrete_helpers
prev_discrete_natural <- function(from, n = 1L) {
  res <- ceiling(from) - seq_len(n)
  res[res < 0] <- NA_real_
  res
}
