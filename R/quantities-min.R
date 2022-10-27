#' @export
range.min <- function(distribution, ...) {
  d <- distribution$components$distributions
  r <- lapply(d, range)
  Reduce(pmin, r)
}
