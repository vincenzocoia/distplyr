#' Probability Mass Function
#'
#' Access a distribution's probability mass function (pmf).
#'
#' @inheritParams get_cdf
#' @return A vector of the evaluated pmf/pdf, in the case of \code{eval_}; a
#'   data frame with both the argument and function evaluations, in the case of
#'   \code{enframe_}; or a vectorized function representing the pmf/pdf, in the
#'   case of \code{get_}.
#' @examples
#' d <- dst_empirical(1:10)
#' eval_pmf(d, at = c(1, 2, 2.5))
#' enframe_pmf(d, at = 0:4)
#' pmf <- get_pmf(d)
#' pmf(0:4)
#' @family distributional representations
#' @rdname pmf
#' @export
eval_pmf <- function(object, at, strict = TRUE) UseMethod("eval_pmf")

#' @rdname pmf
#' @export
get_pmf <- function(object) UseMethod("get_pmf")

#' @export
eval_pmf.dst <- function(object, at, strict = TRUE) {
  if (variable(object) == "discrete") {
    stop("Cannot find the pmf for this distribution.")
  }
  if (strict) {
    stop("This distribution does not have a pmf. ",
         "Maybe you want to evaluate in strict mode?")
  } else {
    if (variable(object) == "continuous") {
      return(rep(0, length(at)))
    } else {
      stop("Cannot find probabilities for this distribution.")
    }
  }
}

#' @export
get_pmf.dst <- function(object) {
  function(at) eval_pmf(object, at = at)
}
