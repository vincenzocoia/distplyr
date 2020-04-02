#' Get Step Points from a Distribution
#'
#' Step points are the coordinates marking the
#' positions of step discontinuities in a
#' distribution. They are the upper points at
#' each discontinuity in the cdf, together
#' with the size of the discontinuity gap.
#'
#' @param object A distribution object.
#' @return Data frame with the following columns:
#' \enumerate{
#'   \item \code{y}: Increasing vector of unique values of \code{y}
#'   having positive weight.
#'   \item \code{prob}: Weights corresponding to each outcome.
#'   \item \code{tau}: Cumulative weights; that is, \code{cumsum(prob)}.
#' }
#' @rdname steps
#' @export
steps <- function(object) UseMethod("steps")

#' @export
steps.dst <- function(object) {
	object[["steps"]]
}
