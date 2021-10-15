#' Probability Density Function
#'
#' Access a distribution's probability density function (pdf).
#'
#' @inheritParams get_cdf
#' @return A vector of the evaluated density, in the case of \code{eval_}; a
#'   data frame with both the argument and function evaluations, in the case of
#'   \code{enframe_}; or a vectorized function representing the density, in the
#'   case of \code{get_}.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_density(d, at = 0:4)
#' enframe_density(d, at = 0:4)
#' density <- get_density(d)
#' density(0:4)
#' @family distributional representations
#' @rdname density
#' @export
eval_density <- function(object, at, strict = TRUE) UseMethod("eval_density")

#' @rdname density
#' @export
get_density <- function(object) UseMethod("get_density")


#' @export
eval_density.dst <- function(object, at, strict = TRUE) {
  if (variable(object) == "continuous") {
    stop("Cannot find this distribution's density function.")
  }
  if (strict) {
    stop("This distribution does not have a density function. ",
         "Maybe you want to evaluate in strict mode?")
  } else {
    if (variable(object) == "discrete") {
      return(rep(0, length(at)))
    } else {
      stop("Cannot find the derivative of the cdf.")
    }
  }
}

#' @export
get_density.dst <- function(object) {
  if (variable(object) != "continuous") {
    return(NULL)
  }
  function(at) eval_density(object, at = at)
}
