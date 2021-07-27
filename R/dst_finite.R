#' Create a Finite Distribution
#'
#' A finite distribution assigns probabilities to a finite
#' collection of values. This includes categorical distributions.
#'
#' @param y <`data-masking`>
#'   Outcomes to comprise the distribution. Should either
#'   evaluate to an (atomic) vector, or be a name in the specified data.
#' @param probs <`data-masking`>
#'   Probabilities corresponding to the outcomes in `y`.
#'   Must not be negative, but **must sum to 1**
#'   (unlike [dst_empirical()]). Should either
#'   evaluate to a vector, or be a name in the specified data.
#' @param data Data frame containing the outcomes `y` and/or
#'   probabilities `probs`. Optional.
#' @param ... Additional arguments, currently not used.
#' @return An object of class `c("finite", "dst")`.
#' @note This distribution is called "finite" and not
#' "discrete", because a discrete distribution could have
#' an infinite amount of possible outcomes, as in the
#' Poisson distribution.
#' @seealso [dst_empirical()]
#' @examples
#' dst_finite(1:5, probs = rep(0.2, 5))
#' @export
dst_finite <- function(y, probs, data, ...) {
  enquo_y <- rlang::enquo(y)
  enquo_w <- rlang::enquo(probs)
  if (missing(data)) {
    y <- rlang::eval_tidy(enquo_y)
    w <- rlang::eval_tidy(enquo_w)
  } else {
    y <- rlang::eval_tidy(enquo_y, data = data)
    w <- rlang::eval_tidy(enquo_w, data = data)
  }
  if (length(y) == 0) {
    warning(
      "Can't make a finite distribution from empty data. ",
      "Returning an empty distribution."
    )
    return(distribution())
  }
  if (length(w) < length(y)) {
    stop("Not enough probabilities to match outcomes `y`.")
  }
  if (length(w) > length(y)) {
    stop("Not enough outcomes `y` to match probabilities.")
  }
  if (any(w < 0, na.rm = TRUE)) {
    stop("Probabilities must not be negative.")
  }
  if (sum(probs) != 1) {
    stop(
      "Probabilities must add up to 1. ",
      "Perhaps you'd prefer to use `dst_empirical()`?"
    )
  }
  steps <- aggregate_weights(y, w, sum_to_one = FALSE)
  if (nrow(steps) == 1L) {
    return(dst_degenerate(steps$location))
  }
  res <- list(probabilities = steps)
  new_finite(res, variable = "discrete")
}


#' Constructor Function for Finite Distributions
#'
#' @param l List containing the components of a step distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#' or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_finite <- function(l, variable, ..., class = character()) {
  new_distribution(
    l,
    variable = variable,
    ...,
    class    = c(class, "finite")
  )
}

#' Is a distribution a finite distribution?
#'
#' There's no difference between checking whether a distribution
#' is finite or empirical -- the functions exist simply for
#' completeness.
#'
#' @param object Object to check
#' @rdname is_finite
#' @export
is_finite_dst <- function(object) inherits(object, "finite")

#' @rdname is_finite
#' @export
is.finite_dst <- function(object) inherits(object, "finite")


#' @export
mean.finite <- function(x, ...) {
  with(x$probabilities, {
    sum(size * location)
  })
}

#' @export
evi.finite <- function(x, ...) {
  NaN
}

#' @export
variance.finite <- function(x, ...) {
  with(x$probabilities, {
    mu <- mean(x)
    mu2 <- sum(size * location^2)
    mu2 - mu^2
  })
}

#' @export
skewness.finite <- function(x, ...) {
  mu <- mean(x)
  sigma <- stdev(x)
  with(x$probabilities, {
    trans <- ((location - mu) / sigma)^3
    sum(trans * size)
  })
}

#' @export
kurtosis_exc.finite <- function(x, ...) {
  mu <- mean(x)
  sigma <- stdev(x)
  with(x$probabilities, {
    trans <- ((location - mu) / sigma)^4
    sum(trans * size) - 3
  })
}

#' @export
get_cdf.finite <- function(object) {
  with(object$probabilities, {
    heights <- c(0, cumsum(size))
    stats::stepfun(location, heights, right = FALSE)
  })
}

#' @export
eval_cdf.finite <- function(object, at) {
  get_cdf(object)(at)
}

#' @export
get_survival.finite <- function(object) {
  with(object$probabilities, {
    heights <- 1 - c(0, cumsum(size))
    stats::stepfun(location, heights, right = FALSE)
  })
}

#' @export
eval_survival.finite <- function(object, at) {
  get_survival(object)(at)
}

#' @export
get_quantile.finite <- function(object, ...) {
  with(object$probabilities, {
    if (identical(length(location), 1L)) {
      function(x) {
        x[!is.na(x) & !is.nan(x)] <- location
        x
      }
    } else {
      taus <- cumsum(size)
      taus <- taus[-length(taus)]
      stats::stepfun(taus, location, right = TRUE)
    }
  })
}

#' @export
eval_quantile.finite <- function(object, at, ...) {
  get_quantile(object)(at)
}

#' @export
realise.finite <- function(object, n = 1, ...) {
  with(object$probabilities, {
    sample(location, size = n, replace = TRUE, prob = size)
  })
}

#' @export
eval_pmf.finite <- function(object, at, strict = TRUE) {
  with(object$probabilities, {
    vapply(at, function(x) sum(size[x == location]), FUN.VALUE = numeric(1L))
  })
}

#' @rdname range
#' @export
range.finite <- function(x, ...) {
  unlisted_probability_list <- x$probabilities$location
  min_val <- min(unlisted_probability_list)
  max_val <- max(unlisted_probability_list)
  c(min_val, max_val)
}

#' @rdname discontinuities
#' @export
discontinuities.finite <- function(object, from = -Inf, to = Inf, ...) {
  if (from > to) {
    stop("'to' argument must be larger or equal than from argument")
  }
  probabilities <- object$probabilities
  location <- probabilities$location
  res <- subset(probabilities, location >= from & location <= to)
  convert_dataframe_to_tibble(res)
}


#' @export
Ops.finite <- function(e1, e2) {
  op <- .Generic[[1]]
  if (is_distribution(e1) && is_distribution(e2)) {
    stop("Operations on two distributions not currently supported.")
  }
  if (is_distribution(e1)) {
    call <- rlang::call2(op, expr(location), e2)
    mutate_finite(e1, !!call)
  } else {
    call <- rlang::call2(op, e1, expr(location))
    mutate_finite(e2, !!call)
  }
}

#' @export
Math.finite <- function(x) {
  f <- .Generic[[1]]
  call <- rlang::call2(f, expr(location))
  mutate_finite(x, !!call)
}
