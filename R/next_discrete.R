#' Probing discrete values in a distribution
#'
#' `next_discrete()` and `prev_discrete()` find the `n` discrete values
#' in a distribution next to a reference point. `num_discretes()` finds
#' the number of discrete values within a range.
#'
#' @param object Distribution
#' @param from Reference value.
#' @param n Number of discrete values to find.
#' @param include_from,include_to Logical; should the `from` value be included
#' in the query? Should the `to` value?
#' @return For `next_discrete()` and `prev_discrete()`, a vector of
#' all available discrete points satisfying the query.
#' If less values are available than asked
#' via `n`, only those values are returned.
#' If infinite values satisfy the query, an error is thrown;
#' `NaN` occurs when no one particular discrete value follows, such as
#' when asking for the integer that comes before infinity.
#'
#' For `num_discretes()`, a single non-negative integer, possibly infinite.
#' @examples
#' next_discrete(dst_pois(1), from = 1.3)
#' prev_discrete(dst_pois(1), from = 3, n = 10)
#' next_discrete(dst_norm(0, 1), from = 1.3, n = 4)
#' @rdname discretes
#' @export
next_discrete <- function(object, from, n = 1L, include_from = FALSE, ...) {
  UseMethod("next_discrete")
}

#' @rdname discretes
#' @export
prev_discrete <- function(object, from, n = 1L, include_from = FALSE, ...) {
  UseMethod("prev_discrete")
}

#' @rdname discretes
#' @export
num_discretes <- function(object, from, to, include_from, include_to) {
  UseMethod("num_discretes")
}

#' @rdname discretes
#' @export
has_infinite_discretes <- function(object, from = -Inf, to = Inf) {
  UseMethod("has_infinite_discretes")
}

#' @export
has_infinite_discretes.dst <- function(object, from = -Inf, to = Inf) {
  if (variable(object) == "continuous") {
    return(FALSE)
  }
  stop("Cannot determine whether this distribution has a finite number ",
       "of discrete values between ", from, " and ", to, ".")
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
prev_discrete.scale <- function(object, from, n = 1L,
                                include_from = FALSE, ...) {
  with(object$components, {
    prev_discrete(distribution,
      from = from,
      n = n,
      include_from = include_from
    ) * scale
  })
}

#' @export
#' @inheritParams next_discrete
next_discrete.inverse <- function(object, from, n, include_from) {
  d_nested <- object$distribution
  if (from >= 0) {
    n_available <- num_discretes(d_nested,
      from = 0, to = 1 / from,
      include_from = FALSE,
      include_to = include_from
    )
    n <- min(n, n_available)
    x <- prev_discrete(d_nested,
      from = 1 / from, n = n,
      include_from = include_from
    )
  } else {
    x <- prev_discrete(d_nested,
      from = 1 / from, n = n,
      include_from = include_from
    )
    n_x <- length(x)
    n_remaining <- n - n_x
    if (n_remaining) {
      n_pos <- num_discretes(d_nested,
        from = 0, to = Inf,
        include_from = FALSE,
        include_to = FALSE
      )
      n_remaining <- min(n_remaining, n_pos)
      x_pos <- prev_discretes(d_nested,
        from = Inf, n = n_remaining,
        include_from = FALSE
      )
      x <- c(x, x_pos)
    }
  }
  1 / x
}

#' @export
#' @inheritParams next_discrete
prev_discrete.inverse <- function(object, from, n, include_from) {
  d_nested <- object$distribution
  if (from <= 0) {
    n_available <- num_discretes(d_nested,
      from = -1 / abs(from), to = 0,
      include_from = include_from,
      include_to = FALSE
    )
    n <- min(n, n_available)
    x <- next_discrete(d_nested,
      from = -1 / abs(from), n = n,
      include_from = include_from
    )
  } else {
    x <- next_discrete(d_nested,
      from = 1 / from, n = n,
      include_from = include_from
    )
    n_x <- length(x)
    n_remaining <- n - n_x
    if (n_remaining) {
      n_neg <- num_discretes(d_nested,
        from = -Inf, to = 0,
        include_from = FALSE,
        include_to = FALSE
      )
      n_remaining <- min(n_remaining, n_neg)
      x_neg <- next_discretes(d_nested,
        from = -Inf, n = n_remaining,
        include_from = FALSE
      )
      x <- c(x, x_neg)
    }
  }
  1 / x
}

#' @export
num_discretes.inverse <- function(object, from, to, include_from, include_to) {
  d_nested <- object$distribution
  if (to < from) {
    a <- to
    b <- from
    include_a <- include_to
    include_b <- include_from
  } else {
    a <- from
    b <- to
    include_a <- include_from
    include_b <- include_to
  }
  if (b <= 0) {
    n <- num_discretes(d_nested,
      from = -1 / abs(b), to = -1 / abs(a),
      include_from = include_b,
      include_to = include_a
    )
  } else if (a >= 0) {
    n <- num_discretes(d_nested,
      from = 1 / b, to = 1 / a,
      include_from = include_b,
      include_to = include_a
    )
  } else {
    n_neg <- num_discretes(d_nested,
      from = -Inf, to = 1 / a,
      include_from = FALSE,
      include_to = include_a
    )
    n_pos <- num_discretes(d_nested,
      from = 1 / b, to = Inf,
      include_from = include_b,
      include_to = FALSE
    )
    n <- n_neg + n_pos
  }
  n
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
  if (from == Inf) {
    return(numeric(0L))
  }
  if (is.infinite(n)) {
    stop("Your selection includes an infinite number of discrete points.")
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
  if (n > 0 && from == Inf) {
    return(NaN)
  }
  ceil_from <- ceiling(from)
  adjust <- from == ceil_from && include_from
  n_max <- max(ceil_from + adjust, 0)
  n <- min(n, n_max)
  ceil_from - seq_len(n) + adjust
}
