#' Hazard Function
#'
#' Access a distribution's hazard function.
#'
#' @inheritParams get_cdf
#' @return A vector of the evaluated hazard, in the case of \code{eval_}; a data
#'   frame with both the argument and function evaluations, in the case of
#'   \code{enframe_}; or a vectorized function representing the hazard, in the
#'   case of \code{get_}.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_hazard(d, at = 0:4)
#' enframe_hazard(d, at = 0:4)
#' hazard <- get_hazard(d)
#' hazard(0:4)
#' @family distributional representations
#' @rdname hazard
#' @export
eval_hazard <- function(object, at) UseMethod("eval_hazard")


#' @rdname hazard
#' @export
get_hazard <- function(object) UseMethod("get_hazard")

#' @export
eval_hazard.dst <- function(object, at) {
  if (variable(object) != "continuous") {
    stop("Hazard function requires a continuous distribution.")
  }
  sf <- eval_survival(object, at)
  pdf <- eval_density(object, at)
  pdf / sf
}

#' @export
get_hazard.dst <- function(object) {
  function(at) eval_hazard(object, at = at)
}


#' @rdname hazard
#' @export
enframe_hazard <- function(object, at,
                           arg_name = ".arg",
                           fn_name = ".hazard") {
  UseMethod("enframe_hazard")
}

#' @export
enframe_hazard.dst <- function(object, at,
                               arg_name = ".arg",
                               fn_name = ".hazard") {
  f <- eval_hazard(object, at = at)
  res <- data.frame(at, f)
  names(res) <- c(arg_name, fn_name)
  convert_dataframe_to_tibble(res)
}
