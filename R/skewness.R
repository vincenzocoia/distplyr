#' Skewness of a Distribution
#'
#' @param x Object to compute skewness
#' @param ... Other arguments to pass to specific methods. Currently not used.
#'
#' @rdname moments
#' @export
skewness <- function(x, ...) UseMethod("skewness")

#' @export
skewness.dst <- function(x, ...) {
  mu <- mean(x)
  sigma <- stdev(x)
  sf <- get_survival(x)
  sf2 <- function(t) sf(mu + t^(1 / 3))
  one_minus_flipped <- function(t) 1 - sf(mu - t^(1 / 3))
  # (flipped about t=0 because (-1)^(1/3) returns a complex root of
  #  unity, or NaN, instead of the real one, -1.)
  pos_int <- stats::integrate(sf2, 0, Inf)
  neg_int <- stats::integrate(one_minus_flipped, 0, Inf)
  (pos_int$value - neg_int$value) / sigma^3
}
