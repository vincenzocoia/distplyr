#' Survival Function
#'
#' Access a distribution's survival function.
#'
#' @inheritParams get_cdf
#' @return A vector of the evaluated survival function, in the case of
#'   \code{eval_}; a data frame with both the argument and function evaluations,
#'   in the case of \code{enframe_}; or a vectorized function representing the
#'   survival function, in the case of \code{get_}.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_survival(d, at = 0:4)
#' enframe_survival(d, at = 0:4)
#' survival <- get_survival(d)
#' survival(0:4)
#' @family distributional representations
#' @rdname survival
#' @export
eval_survival <- function(object, at) UseMethod("eval_survival")

#' @rdname survival
#' @export
get_survival <- function(object) UseMethod("get_survival")


#' @export
get_survival.dst <- function(object) {
  function(at) eval_survival(object, at = at)
}


#' @export
eval_survival.dst <- function(object, at) {
  1 - eval_cdf(object, at = at)
}
