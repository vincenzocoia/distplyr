#' Normal (Gaussian) Distribution
#'
#' Makes a distribution belonging to the family of
#' Normal (Gaussian) distributions.
#' @param mean,variance Mean and variance of the distribution.
#' @return Object of class "dst".
#' dst_norm(0, 1)
#' @export
dst_norm <- function(mean, variance) {
  if (!inherits(variance, "try-error")) {
    if (variance == 0) {
      return(dst_degenerate(mean))
    }
    if (variance < 0) stop("'variance' parameter must be non-negative.")
  }
  res <- list(parameters = list(
    mean = mean,
    variance = variance,
    sd = sqrt(variance)
  ))
  new_parametric(
    res,
    variable = "continuous",
    class    = "norm"
  )
}


#' @export
mean.norm <- function(x, ...) {
  with(parameters(x), mean)
}

#' @export
median.norm <- function(x, ...) {
  with(parameters(x), mean)
}

#' @export
variance.norm <- function(x, ...) {
  with(parameters(x), variance)
}

#' @export
evi.norm <- function(x, ...) {
  0
}

#' @export
skewness.norm <- function(x, ...) {
  0
}

#' @export
kurtosis_exc.norm <- function(x, ...) {
  0
}

#' @export
eval_cdf.norm <- function(object, at) {
  with(parameters(object), {
    stats::pnorm(at, mean = mean, sd = sd)
  })
}

#' @export
eval_survival.norm <- function(object, at) {
  with(parameters(object), {
    stats::pnorm(at, mean = mean, sd = sd, lower.tail = FALSE)
  })
}

#' @export
eval_density.norm <- function(object, at, strict = TRUE) {
  with(parameters(object), {
    stats::dnorm(at, mean = mean, sd = sd)
  })
}

#' @export
realise.norm <- function(object, n = 1, ...) {
  with(parameters(object), {
    stats::rnorm(n, mean = mean, sd = sd)
  })
}

#' @export
eval_quantile.norm <- function(object, at, ...) {
  with(parameters(object), {
    stats::qnorm(at, mean = mean, sd = sd)
  })
}

#' @rdname range
#' @export
range.norm <- function(x, ...) {
  c(-Inf, Inf)
}

# Correct
#' @export
Ops.norm <- function(e1, e2) {
  op <- .Generic[[1]]
  switch(op,
    `+` = {
      if (is_distribution(e1)) {
        mutate_parameters(e1, mean = mean + e2)
      } else {
        mutate_parameters(e2, mean = e1 + mean)
      }
    },
    `-` = {
      if (missing(e2)) {
        mutate_parameters(e1, mean = -mean)
      } else if (is_distribution(e1)) {
        mutate_parameters(e1, mean = mean - e2)
      } else {
        mutate_parameters(e2, mean = e1 - mean)
      }
    },
    `*` = {
      if (is_distribution(e1)) {
        mutate_parameters(e1, mean = mean * e2, variance = variance * e2^2)
      } else {
        mutate_parameters(e1, mean = e1 * mean, variance = e1^2 * variance)
      }
    },
    `/` = {
      if (is_distribution(e1)) {
        mutate_parameters(e1, mean = mean / e2, variance = variance / e2^2)
      } else {
        e1 * make_dst_inverse(e2)
      }
    },
    stop("Operation not currently supported.")
  )
}
