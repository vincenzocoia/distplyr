make_dst_negative <- function(distribution) {
  with(parameters(distribution), {
    dist <- list(
      distribution = distribution
    )
    new_distribution(dist, variable = variable(distribution), class = "negative")
  })
}

#' @export
mean.negative <- function(object, ...) {
  with(object, {
    -mean(distribution)
  })
}

#' @export
median.negative <- function(object, ...) {
  with(object, {
    -median(distribution)
  })
}

#' @export
variance.negative <- function(object, ...) {
  with(object, {
    variance(distribution)
  })
}

#' @export
stdev.negative <- function(object, ...) {
  with(object, {
    stdev(distribution)
  })
}

#' @export
skewness.negative <- function(object, ...) {
  with(object, {
    skewness(distribution)
  })
}

#' @export
kurtosis_exc.negative <- function(object, ...) {
  with(object, {
    kurtosis_exc(distribution)
  })
}

#' @export
realise.negative <- function(object, ...) {
  with(object, {
    -realise(distribution)
  })
}

#' @export
range.negative <- function(object, ...) {
  with(object, {
    d <- range(distribution)
    minimum <- -d[1]
    maximum <- -d[2]
    c(minimum, maximum)
  })
}



#' @export
eval_quantile.negative <- function(object, at, ...) {
  with(object$distribution, {
    -eval_quantile(object, at = 1 - at)
  })
}

#' @export
eval_survival.negative <- function(object, at, ...) {
  with(object$distribution, {
    eval_cdf(distribution, at = -at) -
      eval_pmf(distribution, at = -at, strict = FALSE)
  })
}

#' @export
eval_pmf.negative <- function(object, at, ...) {
  eval_pmf(object$distribution, at = -at, ...)
}

#' @export
eval_density.negative <- function(object, at, ...) {
  eval_density(object$distribution, at = -at, ...)
}

#' @export
eval_cdf.negative <- function(object, at, ...) {
  eval_pmf(object$distribution, at = -at, strict = FALSE) +
    eval_survival(object$distribution, at = -at)
}

# Relying on the .dst functions for the following:
# - eval_hazard
# - eval_chf
