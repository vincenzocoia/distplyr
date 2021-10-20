#' Generate a Sample from a Distribution
#'
#' Draw `n` independent observations from a distribution.
#'
#' @param distribution Distribution.
#' @param n Number of observations to generate
#' @param ... Other arguments to pass to \code{\link{eval_quantile}}
#' @return Vector of independent values drawn from the inputted distribution
#' @note `realise()`, `realize()`, and `observe()` all do the same thing.
#' @rdname realise
#' @export
realise <- function(distribution, n = 1, ...) UseMethod("realise")

#' @export
realise.dst <- function(distribution, n = 1, ...) {
  u <- stats::runif(n)
  return(eval_quantile(distribution, at = u, ...))
}

#' @rdname realise
#' @export
realize <- function(distribution, n = 1) {
	realise(distribution, n = n)
}

#' @rdname realise
#' @export
observe <- function(distribution, n = 1) {
	realise(distribution, n = n)
}
