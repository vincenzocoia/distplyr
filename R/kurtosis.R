#' @rdname moments
#' @export
kurtosis_raw <- function(x, ...) {
  UseMethod("kurtosis_raw")
}

#' @rdname moments
#' @export
kurtosis_exc <- function(x, ...) {
  UseMethod("kurtosis_exc")
}

#' @export
kurtosis_raw.dst <- function(x, ...) {
  3 + kurtosis_exc(x, ...)
}

#' @export
kurtosis_exc.dst <- function(x, ...) {
  mu <- mean(x)
  var <- variance(x)
  sf <- get_survival(x)
  sf2 <- function(t) 1 + sf(mu + t^(1 / 4)) - sf(mu - t^(1 / 4))
  int <- stats::integrate(sf2, 0, Inf)
  int$value / var^2 - 3
}
