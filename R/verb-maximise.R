#' Max Value of Several Distributions
#'
#' For a collection of distributions, this function provides the
#' distribution of the maximum value from independent draws of
#' each component distribution.
#'
#' @inheritParams dots_to_dsts
#' @param draws Number of draws from each distribution considered in the
#' maximum. Either a single numeric applying to all distributions in `...`,
#' or a vector matching the number of distributions in `...`.
#' @return A distribution of class `"max"`.
#' @details To use precise language, if `X1`, ..., `Xp` are
#' `p` independent random variables corresponding to the distributions
#' in `...`, then the distribution returned is of `max(X1, ..., Xp)`.
#' @rdname maximise
#' @export
maximise <- function(..., draws = 1) {
  dsts <- dots_to_dsts(..., na.rm = TRUE)
  n_dsts <- length(dsts)
  if (n_dsts == 0) {
    warning("Received no distributions. Returning NULL.")
    return(NULL)
  }
  draws <- vctrs::vec_recycle(draws, size = n_dsts)
  if (n_dsts == 1 && sum(draws) == 1) {
    return(dsts[[1L]])
  }
  all_finite <- all(vapply(dsts, is_finite_dst, FUN.VALUE = logical(1L)))
  if (all_finite) {
    x <- lapply(dsts, function(d) d$probabilities$location)
    x <- unique(unlist(x))
    upper <- lapply(dsts, prob_left, of = x, inclusive = TRUE)
    lower <- lapply(dsts, prob_left, of = x, inclusive = FALSE)
    contributions_upper <- Map(`^`, upper, draws)
    contributions_lower <- Map(`^`, lower, draws)
    cdf_upper <- Reduce(`*`, contributions_upper)
    cdf_lower <- Reduce(`*`, contributions_lower)
    new_probs <- cdf_upper - cdf_lower
    return(dst_empirical(x, weights = new_probs))
  }
  vars <- unique(unlist(lapply(dsts, variable)))
  if (any(vars == "categorical")) {
    stop("Not meaningful to consider the maximum of a
         categorical distribution.")
  }
  if (length(vars) == 1 && vars != "mixed") {
    v <- vars
  } else {
    r <- lapply(dsts, range)
    mins <- vapply(r, function(r_) r_[1L], FUN.VALUE = numeric(1L))
    largest_min <- max(mins)
    if (is.na(largest_min)) {
      v <- "unknown"
    } else {
      sliced_d <- suppressWarnings(lapply(
        dsts, slice_left, breakpoint = largest_min, include = FALSE
      ))
      vars <- unique(unlist(lapply(sliced_d, variable)))
      if (length(vars) == 1) {
        v <- vars
      } else if (any(vars == "unknown")) {
        v <- "unknown"
      } else {
        v <- "mixed"
      }
    }
  }
  l <- list(components = list(distributions = dsts, draws = draws))
  distionary::new_distribution(l, variable = v, class = "max")
}

#' @rdname maximise
#' @export
maximize <- maximise
