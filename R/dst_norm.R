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
