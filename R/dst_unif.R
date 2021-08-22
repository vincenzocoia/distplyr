#' Make a Uniform distribution
#'
#' Makes a distribution belonging to the continuous uniform family of
#' distributions. Note: this distribution has been adapted to trial
#' a tidy evaluation framework.
#' @param min,max Parameters of the distribution family. Need not specify
#' anything (uses tidy evaluation).
#' @return Object of class "dst".
#' dst_unif(0, 1)
#' @export
dst_unif <- function(min, max) {
  if (missing(min)) {
    min <- rlang::expr(.min)
    resolved_min <- FALSE
  } else {
    min <- resolve_if_possible({{ min }})
    resolved_min <- min$resolved
    min <- min$outcome
  }
  if (resolved_min && !is.numeric(min)) {
    stop(
      "'min' argument should be numeric, but received ",
      class(min),
      "."
    )
  }
  if (missing(max)) {
    max <- rlang::expr(.max)
    resolved_max <- FALSE
  } else {
    max <- resolve_if_possible({{ max }})
    resolved_max <- max$resolved
    max <- max$outcome
  }
  if (resolved_max && !is.numeric(max)) {
    stop(
      "'max' argument should be numeric, but received ",
      class(max),
      "."
    )
  }
  if (resolved_min && resolved_max) {
    if (max < min) {
      stop("Parameter 'min' must be less than 'max'.")
    }
    if (max == min) {
      return(dst_degenerate(min))
    }
  }
  res <- list(parameters = list(
    min = min,
    max = max
  ))
  new_parametric(res,
    variable = "continuous",
    class = "unif"
  )
}

#' Resolve an input if possible
#'
#' This function tries to evaluate its argument, providing
#' the outcome if succeeding, and a quosure if failing.
#'
#' @param x A line of code.
#'
#' @return A list: the \code{$outcome} entry contains the
#' evaluated input if evaluation is possible, or a quosure
#' of the input if not possible; \code{$resolved} is a
#' logical entry indicating whether or not the input was
#' able to be resolved (\code{TRUE} if so, \code{FALSE} if not).
#'
#' @examples
#' resolve_if_possible(cowabunga)
#' resolve_if_possible(42)
#' cowabunga <- 42
#' resolve_if_possible(cowabunga)
resolve_if_possible <- function(x) {
  x <- rlang::enquo(x)
  try_x <- try(rlang::eval_tidy(x), silent = TRUE)
  resolved <- !inherits(try_x, "try-error")
  if (resolved) {
    x <- try_x
  }
  list(outcome = x, resolved = resolved)
}
