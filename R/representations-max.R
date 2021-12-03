#' @export
eval_cdf.max <- function(distribution, at) {
  d <- distribution$components$distributions
  cdfs <- lapply(d, eval_cdf, at = at)
  Reduce(`*`, cdfs)
}

#' @export
eval_density.max <- function(distribution, at) {
  d <- distribution$components$distributions
  full_cdf <- eval_cdf(distribution, at = at)
  cdfs <- lapply(d, eval_cdf, at = at)
  pdfs <- lapply(d, eval_density, at = at)
  ratios <- Map(`/`, pdfs, cdfs)
  ratios_sum <- Reduce(`+`, ratios)
  ratios_sum * full_cdf
}

#' @export
realise.max <- function(distribution, n = 1) {
  d <- distribution$components$distributions
  draws <- vapply(d, realise, FUN.VALUE = numeric(1L))
  max(draws)
}
