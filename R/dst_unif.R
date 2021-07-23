#' Make a Uniform distribution
#'
#' Makes a distribution belonging to the continuous uniform family of
#' distributions. Note: this distribution has been adapted to trial
#' a tidy evaluation framework.
#' @param min,max Parameters of the distribution family. Need not specify
#' anything (uses tidy evaluation).
#' @return Object of class "dst".
#' dst_unif(0, 1)
#' @export
dst_unif <- function(min, max) {
  if (missing(min)) {
    min <- rlang::expr(.min)
    resolved_min <- FALSE
  } else {
    min <- resolve_if_possible({{ min }})
    resolved_min <- min$resolved
    min <- min$outcome
  }
  if (resolved_min && !is.numeric(min)) {
    stop(
      "'min' argument should be numeric, but received ",
      class(min),
      "."
    )
  }
  if (missing(max)) {
    max <- rlang::expr(.max)
    resolved_max <- FALSE
  } else {
    max <- resolve_if_possible({{ max }})
    resolved_max <- max$resolved
    max <- max$outcome
  }
  if (resolved_max && !is.numeric(max)) {
    stop(
      "'max' argument should be numeric, but received ",
      class(max),
      "."
    )
  }
  if (resolved_min && resolved_max) {
    if (max < min) {
      stop("Parameter 'min' must be less than 'max'.")
    }
    if (max == min) {
      return(dst_degenerate(min))
    }
  }
  res <- list(parameters = list(
    min = min,
    max = max
  ))
  new_parametric(res,
    variable = "continuous",
    class = "unif"
  )
}

#' Resolve an input if possible
#'
#' This function tries to evaluate its argument, providing
#' the outcome if succeeding, and a quosure if failing.
#'
#' @param x A line of code.
#'
#' @return A list: the \code{$outcome} entry contains the
#' evaluated input if evaluation is possible, or a quosure
#' of the input if not possible; \code{$resolved} is a
#' logical entry indicating whether or not the input was
#' able to be resolved (\code{TRUE} if so, \code{FALSE} if not).
#'
#' @examples
#' resolve_if_possible(cowabunga)
#' resolve_if_possible(42)
#' cowabunga <- 42
#' resolve_if_possible(cowabunga)
resolve_if_possible <- function(x) {
  x <- rlang::enquo(x)
  try_x <- try(rlang::eval_tidy(x), silent = TRUE)
  resolved <- !inherits(try_x, "try-error")
  if (resolved) {
    x <- try_x
  }
  list(outcome = x, resolved = resolved)
}





#' @export
mean.unif <- function(x, ...) {
  with(parameters(x), {
    res <- resolve_if_possible((!!min + !!max) / 2)
    if (res$resolved) {
      res$outcome
    } else {
      rlang::get_expr(res$outcome)
    }
  })
}

#' @export
median.unif <- function(x, ...) {
  with(parameters(x), (min + max) / 2)
}

#' @export
variance.unif <- function(x, ...) {
  with(parameters(x), (min - max)^2 / 12)
}


#' @export
evi.unif <- function(x, ...) {
  -1
}

#' @export
skewness.unif <- function(x, ...) {
  0
}

#' @export
kurtosis_exc.unif <- function(x, ...) {
  -6 / 5
}

#' @export
eval_cdf.unif <- function(object, at) {
  with(parameters(object), {
    stats::punif(at, min = min, max = max)
  })
}

#' @export
eval_survival.unif <- function(object, at) {
  with(parameters(object), {
    stats::punif(at,
      min = min,
      max = max,
      lower.tail = FALSE
    )
  })
}

#' @export
eval_density.unif <- function(object, at, strict = TRUE) {
  with(parameters(object), {
    res <-
      resolve_if_possible(stats::dunif(!!at, min = !!min, max = !!max))
    if (res$resolved) {
      res$outcome
    } else {
      rlang::get_expr(res$outcome)
    }
  })
}

#' @export
realise.unif <- function(object, n = 1, ...) {
  with(parameters(object), {
    stats::runif(n, min = min, max = max)
  })
}

#' @export
eval_quantile.unif <- function(object, at, ...) {
  with(parameters(object), {
    stats::qunif(at, min = min, max = max)
  })
}


#' @rdname range
#' @export
range.unif <- function(x, ...) {
  with(parameters(x), {
    c(min, max)
  })
}

#' @rdname discontinuities
#' @export
discontinuities.unif <- function(object, from = -Inf, to = Inf, ...) {
  if (from > to) {
    stop("'to' argument must be larger or equal than from argument")
  }
  make_empty_discontinuities_df()
}

#' @export
Ops.unif <- function(e1, e2) {
  op <- .Generic[[1]]
  switch(op,
    `+` = {
      if (inherits(e1, "unif")) {
        make_unif(e1, e2, `+`, `+`)
      } else {
        make_unif(e2, e1, `+`, `+`)
      }
    },
    `-` = {
      if (missing(e2)) {
        with(parameters(e1), {
          dst_unif(-max, -min)
        })
      } else if (inherits(e1, "unif")) {
        make_unif(e1, e2, `-`, `-`)
      } else {
        with(parameters(e2), {
          dst_unif(e1 - max, e1 - min)
        })
      }
    },
    `*` = {
      if (inherits(e1, "unif")) {
        d <- e1
        cnst <- e2
      } else {
        d <- e2
        cnst <- e1
      }
      if (cnst < 0) {
        return(-cnst * (-d))
      }
      make_unif(d, cnst, `*`, `*`)
    },
    `/` = {
      if (inherits(e1, "unif")) {
        make_unif(e1, e2, `/`, `/`)
      } else {
        make_unif(e2, e1, `/`, `/`)
      }
    },
    stop("Not a valid Operation")
  )
}

make_unif <- function(distribution, constant, FUN, FUN2) {
  with(parameters(distribution), {
    dst_unif(FUN(min, constant), FUN2(max, constant))
  })
}
