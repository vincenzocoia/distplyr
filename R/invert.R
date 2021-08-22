#' @export
invert <- function(distribution) {
  p_zero <- eval_pmf(distribution, at = 0, strict = FALSE)
  if (p_zero > 0) {
    stop("Cannot invert a distribution for which 0 is a possible outcome.")
  }
  dist <- list(
    distribution = distribution
  )
  new_distribution(dist, variable = variable(distribution), class = "inverse")
}
