#' Min Value of Several Distributions
#'
#' For a collection of distributions, this function provides the
#' distribution of the minimum value from independent draws of
#' each component distribution.
#'
#' @param ... Distribution objects
#' @param draws Number of draws from each distribution considered in the
#' minimum. Either a single numeric applying to all distributions in `...`,
#' or a vector matching the number of distributions in `...`.
#' @return A distribution of class `"min"`.
#' @details To use precise language, if `X1`, ..., `Xp` are
#' `p` independent random variables corresponding to the distributions
#' in `...`, then the distribution returned is of `min(X1, ..., Xp)`.
#' @rdname minimise
#' @export
minimise <- function(..., draws = 1) {
  dsts <- rlang::quos(...)
  dsts <- lapply(dsts, rlang::eval_tidy)
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
    upper <- lapply(dsts, prob_right, of = x, inclusive = TRUE)
    lower <- lapply(dsts, prob_right, of = x, inclusive = FALSE)
    contributions_upper <- Map(`^`, upper, draws)
    contributions_lower <- Map(`^`, lower, draws)
    surv_upper <- Reduce(`*`, contributions_upper)
    surv_lower <- Reduce(`*`, contributions_lower)
    new_probs <- surv_upper - surv_lower
    return(dst_empirical(x, weights = new_probs))
  }
  vars <- unique(unlist(lapply(dsts, variable)))
  if (any(vars == "categorical")) {
    stop("Not meaningful to consider the minimum of a
         categorical distribution.")
  }
  if (length(vars) == 1 && vars != "mixed") {
    v <- vars
  } else {
    r <- lapply(dsts, range)
    maxs <- vapply(r, function(r_) r_[2L], FUN.VALUE = numeric(1L))
    smallest_max <- min(maxs)
    if (is.na(smallest_max)) {
      v <- "unknown"
    } else {
      sliced_d <- suppressWarnings(lapply(
        dsts, slice_right, breakpoint = smallest_max, include = FALSE
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
  distionary::new_distribution(l, variable = v, class = "min")
}

#' @rdname minimise
#' @export
minimize <- minimise
