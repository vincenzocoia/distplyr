#' Poisson Distribution
#'
#' Makes a distribution belonging to the family of
#' Poisson distributions.
#' @param lambda Rate of Occurance for distribution
#' @return Object of class "dst".
#' dst_poisson(1)
#' @export
dst_pois <- function(lambda) {
  if (lambda < 0) {
    stop("'lambda' parameter must greater than 0")
  } else if (lambda == 0) {
    return(dst_degenerate(lambda))
  }
  res <- list(parameters = list(
    lambda = lambda
  ))
  new_parametric(res, variable = "discrete", class = "pois")
}
