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

#' @rdname pmf
#' @export
enframe_pmf <- function(object, at,
                        arg_name = ".arg",
                        fn_name = ".pmf") {
  UseMethod("enframe_pmf")
}


#' @export
eval_pmf.dst <- function(object, at, strict = TRUE) {
  # TODO: What is the need for the discontinuites?
  # TODO: What do we do if strict = false and we need to calculate shit?
  if (variable(object) == "continuous") {
    stop("Cannot Evaluate pmf of continuous variable")
  } else if (variable(object) == "mixed" && strict) {
    stop("Cannot Evaluate pmf of mixed variable in strict mode")
  } else {
    eval_pmf()
    rng <- range(at)
    discretes <- discontinuities(object, from = rng[1], to = rng[2])
    with(discretes, {
      vapply(at, function(x) sum(size[x == location]), FUN.VALUE = numeric(1L))
    })
  }
}

#' @export
get_pmf.dst <- function(object) {
  function(at) eval_pmf(object, at = at)
}

#' @export
enframe_pmf.dst <- function(object, at,
                            arg_name = ".arg",
                            fn_name = ".pmf") {
  f <- eval_pmf(object, at = at)
  res <- data.frame(at, f)
  names(res) <- c(arg_name, fn_name)
  convert_dataframe_to_tibble(res)
}
