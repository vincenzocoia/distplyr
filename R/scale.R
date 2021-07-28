#' This helper function is not anticipating a negative constant.
make_dst_scale <- function(distribution, constant) {
  with(parameters(distribution), {
    if (constant < 0) {
      stop("make_dst_scale cannot accept negative constant")
    } else if (constant == 0) {
      dst_degenerate(0)
    } else if (constant == 1) {
      distribution
    } else {
      dist <- list(
        components = list(
          distribution = distribution,
          scale = constant
        )
      )
      new_distribution(dist, variable = variable(e1), class = "scale")
    }
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
eval_pmf.scale <- function(object, at, strict = TRUE, ...) {
  with(object$components, {
    eval_pmf(distribution, at / scale, strict = strict, ...)
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

realise.scale <- function(object, ...) {
  with(object$components, {
    eval_survival(distribution, ...) * scale
  })
}