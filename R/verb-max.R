#' Max Value of Several Distributions
#'
#' For a collection of distributions, this function provides the
#' distribution of the maximum value from independent draws of
#' each component distribution.
#'
#' @param ... Distribution objects
#' @return A distribution of class `"max"`.
#' @details To use precise language, if `X1`, ..., `Xp` are
#' `p` independent random variables corresponding to the distributions
#' in `...`, then the distribution returned is of `max(X1, ..., Xp)`.
#' @export
maximise <- function(...) {
  dsts <- rlang::quos(...)
  dsts <- lapply(dsts, rlang::eval_tidy)
  if (length(dsts) == 1) return(dsts[[1L]])
  l <- list(components = list(distributions = dsts))
  vars <- vapply(dsts, distionary::variable, FUN.VALUE = character(1L))
  vars <- unique(vars)
  if (any(vars == "categorical")) {
    stop("Not meaningful to consider the maximum of a
         categorical distribution.")
  }
  if (length(vars) == 1) {
    v <- vars
  } else if (any(vars == "unknown")) {
    v <- "unknown"
  } else {
    v <- "mixed"
  }
  distionary::new_distribution(l, variable = v, class = "max")
}


maximize <- maximise
