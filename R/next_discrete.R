#' Return neighbouring discrete values from a reference
#'
#' Finds the next `n` largest/smallest discrete values in a distribution
#' from some reference point.
#'
#' @param object Distribution
#' @param from Reference value.
#' @param n Number of discrete values to find.
#' @param include_from Logical; should the `from` value be included
#' in the query?
#' @return Vector of all available discrete points satisfying the query.
#' If infinite values satify the query, an error is thrown; if less
#' values are available than asked via `n`, only those values are returned.
#' @examples
#' next_discrete(dst_pois(1), from = 1.3)
#' prev_discrete(dst_pois(1), from = 3, n = 10)
#' next_discrete(dst_norm(0, 1), from = 1.3, n = 4)
#' @export
next_discrete <- function(object, from, n = 1L, include_from = FALSE, ...) {
  UseMethod("next_discrete")
}

#' @export
prev_discrete <- function(object, from, n = 1L, include_from = FALSE, ...) {
  UseMethod("prev_discrete")
}

#' @export
#' @inheritParams next_discrete
next_discrete.dst <- function(object, from, n = 1L,
                              include_from = FALSE, ...) {
  if (variable(object) == "continuous") {
    return(numeric(0L))
  }
  stop("Cannot find the next discrete values for this distribution.")
}

#' @export
#' @inheritParams next_discrete
prev_discrete.dst <- function(object, from, n = 1L,
                              include_from = FALSE, ...) {
  if (variable(object) == "continuous") {
    return(numeric(0L))
  }
  stop("Cannot find the previous discrete values for this distribution.")
}

#' @export
#' @inheritParams next_discrete
next_discrete.pois <- function(object, from, n = 1L,
                               include_from = FALSE, ...) {
  next_discrete_natural(from, n = n, include_from = include_from)
}

#' @export
#' @inheritParams next_discrete
prev_discrete.pois <- function(object, from, n = 1L,
                               include_from = FALSE, ...) {
  prev_discrete_natural(from, n = n, include_from = include_from)
}

#' @export
#' @inheritParams next_discrete
next_discrete.finite <- function(object, from, n = 1L,
                                 include_from = FALSE, ...) {
  all_discretes <- object$probabilities$location
  next_discrete_finite(all_discretes,
    from = from, n = n,
    include_from = include_from
  )
}

#' @export
#' @inheritParams next_discrete
prev_discrete.finite <- function(object, from, n = 1L,
                                 include_from = FALSE, ...) {
  all_discretes <- object$probabilities$location
  prev_discrete_finite(all_discretes,
    from = from, n = n,
    include_from = include_from
  )
}

#' @export
#' @inheritParams next_discrete
next_discrete.degenerate <- function(object, from, n = 1L,
                                     include_from = FALSE, ...) {
  x <- parameters(object)[["location"]]
  next_discrete_finite(x,
    from = from, n = n,
    include_from = include_from
  )
}

#' @export
#' @inheritParams next_discrete
prev_discrete.degenerate <- function(object, from, n = 1L,
                                     include_from = FALSE, ...) {
  x <- parameters(object)[["location"]]
  prev_discrete_finite(x, from = from, n = n, include_from = include_from)
}

#' @export
#' @inheritParams next_discrete
next_discrete.shift <- function(object, from, n = 1L,
                                include_from = FALSE, ...) {
  with(object$components, {
    next_discrete(distribution,
      from = from,
      n = n,
      include_from = include_from
    ) + shift
  })
}

#' @export
#' @inheritParams next_discrete
prev_discrete.shift <- function(object, from, n = 1L,
                                include_from = FALSE, ...) {
  with(object$components, {
    prev_discrete(distribution,
      from = from,
      n = n,
      include_from = include_from
    ) + shift
  })
}

#' @export
#' @inheritParams next_discrete
next_discrete.scale <- function(object, from, n = 1L,
                                include_from = FALSE, ...) {
  with(object$components, {
    next_discrete(distribution,
      from = from,
      n = n,
      include_from = include_from
    ) * scale
  })
}

#' @export
#' @inheritParams next_discrete
prev_discrete.shift <- function(object, from, n = 1L,
                                include_from = FALSE, ...) {
  with(object$components, {
    prev_discrete(distribution,
      from = from,
      n = n,
      include_from = include_from
    ) * scale
  })
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
next_discrete_finite <- function(x, from, n, include_from) {
  if (include_from) {
    query <- x >= from
  } else {
    query <- x > from
  }
  higher_discretes <- x[query]
  n <- min(length(higher_discretes), n)
  sort(higher_discretes)[seq_len(n)]
}

#' @rdname discrete_helpers
prev_discrete_finite <- function(x, from, n, include_from) {
  if (include_from) {
    query <- x <= from
  } else {
    query <- x < from
  }
  lower_discretes <- x[query]
  n <- min(length(lower_discretes), n)
  sort(lower_discretes, decreasing = TRUE)[seq_len(n)]
}

#' @rdname discrete_helpers
next_discrete_natural <- function(from, n, include_from) {
  if (is.infinite(n)) {
    stop("Your selection includes and infinite number of discrete points.")
  }
  if (from < 0) {
    return(seq_len(n) - 1L)
  }
  floor_from <- floor(from)
  adjust <- from == floor_from && include_from
  floor_from + seq_len(n) - adjust
}

#' @rdname discrete_helpers
prev_discrete_natural <- function(from, n, include_from) {
  ceil_from <- ceiling(from)
  adjust <- from == ceil_from && include_from
  n_max <- max(ceil_from + adjust, 0)
  n <- min(n, n_max)
  ceil_from - seq_len(n) + adjust
}