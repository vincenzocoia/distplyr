#' Cumulative Hazard Function
#'
#' Access a distribution's cumulative hazard function (chf).
#'
#' @inheritParams get_cdf
#' @return A vector of the evaluated chf, in the case of \code{eval_}; a data
#'   frame with both the argument and function evaluations, in the case of
#'   \code{enframe_}; or a vectorized function representing the chf, in the case
#'   of \code{get_}.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_chf(d, at = 0:4)
#' enframe_chf(d, at = 0:4)
#' chf <- get_chf(d)
#' chf(0:4)
#' @family distributional representations
#' @rdname chf
#' @export
get_chf <- function(object) UseMethod("get_chf")

#' @rdname chf
#' @export
eval_chf <- function(object, at) UseMethod("eval_chf")

#' @export
eval_chf.dst <- function(object, at) {
  if (variable(object) == "continuous") {
    sf <- eval_survival(object, at = at)
    -log(sf)
  } else {
    stop("Not programmed yet")
  }
}

#' @export
get_chf.dst <- function(object) {
  function(at) eval_chf(object, at = at)
}
