#' Generate a Sample from a Distribution
#'
#' @param distribution Distribution.
#' @param n Number of observations to generate
#' @param ... Other arguments to pass to \code{\link{eval_quantile}}
#' @return Vector of independent values drawn from the inputted distribution
#' @rdname realise
#' @export
realise <- function(distribution, n = 1, ...) UseMethod("realise")

#' @rdname realise
#' @export
realize <- function(distribution, n = 1, ...) UseMethod("realise")

#' @export
realise.dst <- function(distribution, n = 1, ...) {
  u <- stats::runif(n)
  return(eval_quantile(distribution, at = u, ...))
}

#' @export
realise.default <- function(distribution, n = 1, ...) {
  rep(distribution, n)
}
