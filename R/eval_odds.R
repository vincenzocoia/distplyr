#' Odds Function
#'
#' Access a distribution's odds function. The odds of an event having
#' probability `p` is `p / (1 - p)`.
#'
#' @inheritParams get_cdf
#' @return A vector of the evaluated odds, in the case of \code{eval_}; a
#'   data frame or tibble with both the argument and function evaluations,
#'   in the case of \code{enframe_};
#'   or a vectorized function representing the odds function, in the
#'   case of \code{get_}.
#' @examples
#' d <- dst_pois(1)
#' eval_odds(d, at = c(1, 2, 2.5))
#' enframe_odds(d, at = 0:4)
#' eval_odds(dst_norm(0, 1), at = 0:4, strict = FALSE)
#' @family distributional representations
#' @rdname odds
#' @export
eval_odds <- function(object, at) {
	p <- eval_pmf(object, at = at, strict = FALSE)
	p / (1 - p)
}
