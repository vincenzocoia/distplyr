#' Make a Degenerate Distribution
#'
#' Makes a distribution belonging to the degenerate family of
#' distributions. That is, distributions of fixed values.
#' @param location Parameter of the distribution family.
#' @return Object of class "dst".
#' @examples
#' require(graphics)
#' d <- dst_degenerate(5)
#' plot(d, "quantile")
#' @rdname degenerate
#' @export
dst_degenerate <- function(location) {
  if (!is.numeric(location)) {
    stop("'location' parameter must be numeric.")
  }
  if (length(location) != 1L) {
    stop(
      "'location' parameter must contain exactly one number. ",
      "Received ", length(location)
    )
  }
  as_param <- list(location = location)
  as_table <- aggregate_weights(location, 1, sum_to_one = FALSE)
  res <- list(
    parameters = as_param,
    probabilities = as_table
  )
  new_parametric(res,
    variable = "discrete",
    class = c("degenerate", "finite")
  )
}

#' @param object Object to test
#' @rdname degenerate
#' @export
is_degenerate <- function(object) {
  inherits(object, "degenerate")
}

#' @rdname degenerate
#' @export
is.degenerate <- function(object) {
  inherits(object, "degenerate")
}

#' @export
mean.degenerate <- function(x, ...) {
  parameters(x)$location
}

#' @export
median.degenerate <- function(x, ...) {
  parameters(x)$location
}

#' @export
variance.degenerate <- function(x, ...) {
  0
}

#' @export
sd.degenerate <- function(x, ...) {
  0
}

#' @export
skewness.degenerate <- function(x, ...) {
  NaN
}

#' @export
kurtosis_exc.degenerate <- function(x, ...) {
  NaN
}

#' @export
realise.degenerate <- function(object, n = 1, ...) {
  rep(parameters(object)$location, n)
}

#' @rdname range
#' @export
range.degenerate <- function(x, ...) {
  location <- parameters(x)$location
  c(location, location)
}

#' @rdname discontinuities
#' @export
discontinuities.degenerate <- function(object, from = -Inf, to = Inf, ...) {
  with(parameters(object), {
    if (from > to) {
      stop("To argument must be larger or equal than from argument")
    }
    if (from <= location & to >= location) {
      res <- data.frame(location = location, size = 1)
    } else {
      res <- make_empty_discontinuities_df()
    }
    convert_dataframe_to_tibble(res)
  })
}

# Using .finite method for:
# - all functional representations (cdf, hazard, etc.), except random number
#    generator.
# - evi
