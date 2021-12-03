#' @export
range.max <- function(distribution, ...) {
  d <- distribution$components$distributions
  r <- lapply(d, range)
  a <- Reduce(pmin, r)[1L]
  b <- Reduce(pmax, r)[2L]
  c(a, b)
}
