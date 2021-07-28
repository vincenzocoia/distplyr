#' Poisson Distribution
#'
#' Makes a distribution belonging to the family of
#' Poisson distributions.
#' @param lambda Rate of Occurance for distribution
#' @return Object of class "dst".
#' dst_poisson(1)
#' @export
dst_pois <- function(lambda) {
  if (lambda < 0) {
    stop("'lambda' parameter must greater than 0")
  } else if (lambda == 0) {
    return(dst_degenerate(lambda))
  }
  res <- list(parameters = list(
    lambda = lambda
  ))
  new_parametric(res, variable = "discrete", class = "pois")
}


#' @export
mean.pois <- function(x, ...) {
  with(parameters(x), lambda)
}

#' @export
variance.pois <- function(x, ...) {
  with(parameters(x), lambda)
}

#' @export
evi.pois <- function(x, ...) {
  NaN
}

#' @export
skewness.pois <- function(x, ...) {
  with(parameters(x), lambda^(-0.5))
}

#' @export
kurtosis_exc.pois <- function(x, ...) {
  with(parameters(x), lambda^(-1))
}

#' @export
eval_cdf.pois <- function(object, at) {
  with(parameters(object), {
    stats::ppois(at, lambda = lambda)
  })
}

#' @export
eval_survival.pois <- function(object, at) {
  with(parameters(object), {
    stats::ppois(at, lambda = lambda, lower.tail = FALSE)
  })
}


#' @export
eval_pmf.pois <- function(object, at, strict = TRUE) {
  with(parameters(object), {
    suppressWarnings(stats::dpois(at, lambda = lambda))
  })
}

#' @export
realise.pois <- function(object, n = 1, ...) {
  with(parameters(object), {
    stats::rpois(n, lambda = lambda)
  })
}

#' @export
eval_quantile.pois <- function(object, at, ...) {
  with(parameters(object), {
    stats::qpois(at, lambda = lambda)
  })
}

#' @rdname range
#' @export
range.pois <- function(x, ...) {
  c(0, Inf)
}
