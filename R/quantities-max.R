#' @export
range.max <- function(distribution, ...) {
  d <- distribution$components$distributions
  r <- lapply(d, range)
  Reduce(pmax, r)
}
