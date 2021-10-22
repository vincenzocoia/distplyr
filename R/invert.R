#' Inverse Distribution
#'
#' Access the inverse of a distribution. Specifically, if `X` is a random
#' variable coming from a distribution, `invert()` returns the
#' distribution of `1 / X`.
#'
#' @param distribution A probability distribution.
#' @return An inverse distribution.
#' Specifically, a distribution with subclass "inverse".
#' @note An error is returned if the original distribution
#' has 0 as a possible outcome
#' (i.e., `eval_pmf(distribution, at = 0, strict = FALSE)` is non-zero),
#' because 0 does not have a reciprocal.
#'
#' You can also obtain the inverse distribution by putting
#' the distribution in the denominator of `/`.
#' @seealso `flip()`, `scale()`
#' @examples
#' 1 / (dst_pois(3.4) + 1)
#' invert(dst_norm(0, 1))
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
