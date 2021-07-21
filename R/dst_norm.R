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

#' @rdname discontinuities
#' @export
discontinuities.norm <- function(object, from = -Inf, to = Inf, ...) {
  if (from > to) {
    stop("'to' argument must be larger or equal than from argument")
  }
  make_empty_discontinuities_df()
}

# Correct
#' @export
Ops.norm <- function(e1, e2) {
  op <- .Generic[[1]]
  switch(op,
    `+` = {
      if (inherits(e1, "norm")) {
        make_norm(e1, e2, `+`, function(x, y) x)
      } else {
        make_norm(e2, e1, `+`, function(x, y) x)
      }
    },
    `-` = {
      if (missing(e2)) {
        make_norm(e1, 0, function(x, y) -x, function(x, y) x)
      } else if (inherits(e1, "norm")) {
        make_norm(e1, e2, `-`, function(x, y) x)
      } else {
        make_norm(e2, e1, function(x, y) y - x, function(x, y) x)
      }
    },
    `*` = {
      if (inherits(e1, "norm")) {
        make_norm(e1, e2, function(x, y) x, `*`)
      } else {
        make_norm(e2, e1, function(x, y) x, `*`)
      }
    },
    `/` = {
      if (inherits(e1, "norm")) {
        make_norm(e1, e2, function(x, y) x, `/`)
      } else {
        if (e2 == 1) {
          recp <- make_dst_inverse(recp, e2)
        } else if (e2 < 0) {
          recp <- make_dst_scale(make_dst_negative(make_dst_inverse(e1)), -e2)
        } else {
          recp <- make_dst_scale(make_dst_inverse(e1), e2)
        }
        recp
      }
    },
    stop("Not a valid Operation")
  )
}

make_norm <- function(e1, e2, FUN, FUN2) {
  with(parameters(e1), {
    dst_norm(FUN(mean, e2), FUN2(variance, e2))
  })
}

#' @examples
#' d <- dst_norm(0, 1)
#' transform_parameters(d, list(variance = function(x) x * 2))
transform_parameters <- function(distribution, list_of_functions) {

}


# Using .dst method for:
# - get_hazard
# - get_chf