#' This helper function is not anticipating a negative constant.
make_dst_scale <- function(distribution, constant) {
  with(parameters(distribution), {
    dist <- list(
      components = list(
        distribution = distribution,
        scale = constant
      )
    )
    res <- structure(
      dist,
      variable = variable(distribution),
      class = c(class(d)[1], "negative", class(distribution)[-1])
    )
  })
}

#' @export
mean.scale <- function(object) {
  with(object$components, {
    mean(distribution) * scale
  })
}

#' @export
median.scale <- function(object) {
  with(object$components, {
    median(distribution) * scale
  })
}

#' @export
stdev.scale <- function(object) {
  with(object$components, {
    stdev(distribution) * scale
  })
}

#' @export
range.scale <- function(object) {
  with(object$components, {
    range(distribution) * scale
  })
}

#' @export
variance.scale <- function(object) {
  with(object$components, {
    variance(distribution) * scale^2
  })
}

#' @export
evi.scale <- function(object) {
  with(object$components, {
    evi(distribution)
  })
}

#' @export
skewness.scale <- function(object) {
  with(object$components, {
    skewness(distribution)
  })
}

#' @export
kurtosis_exc.scale <- function(object) {
  with(object$components, {
    kurtosis_exc(distribution)
  })
}

#' @export
eval_cdf.scale <- function(object, at) {
  with(object$components, {
    eval_cdf(distribution, at = at / scale)
  })
}

#' @export
eval_quantile.scale <- function(object, at) {
  with(object$components, {
    eval_quantile(distribution, at) * scale
  })
}

#' @export
eval_pmf.scale <- function(object, at) {
  with(object$components, {
    eval_pmf(distribution, at / scale)
  })
}

#' @export
eval_density.scale <- function(object, at) {
  with(object$components, {
    eval_density(distribution, at / scale) / scale
  })
}

#' @export
eval_survival.scale <- function(object, at) {
  with(object$components, {
    eval_survival(distribution, at / scale)
  })
}