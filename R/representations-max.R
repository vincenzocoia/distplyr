#' @export
prob_left.max <- function(distribution, of, inclusive) {
  d <- distribution$components$distributions
  draws <- distribution$components$draws
  prob_lefts <- lapply(d, prob_left, of = of, inclusive = inclusive)
  contributions <- Map(`^`, prob_lefts, draws)
  Reduce(`*`, contributions)
}

#' @export
eval_cdf.max <- function(distribution, at) {
  prob_left(distribution, of = at, inclusive = TRUE)
}

#' @export
eval_density.max <- function(distribution, at, strict = TRUE) {
  # formula: cdf * (sum draws_j f_j / F_j)
  d <- distribution$components$distributions
  draws <- distribution$components$draws
  full_cdf <- eval_cdf(distribution, at = at)
  cdfs <- lapply(d, eval_cdf, at = at)
  pdfs <- lapply(d, eval_density, at = at, strict = strict)
  divide_if_nonzero <- function(draws, pdf, cdf) {
    res <- draws * pdf / cdf
    res[pdf == 0] <- 0
    res
  }
  ratios <- Map(divide_if_nonzero, draws, pdfs, cdfs)
  ratios_sum <- Reduce(`+`, ratios)
  ratios_sum * full_cdf
}

#' @export
eval_pmf.max <- function(distribution, at, strict = TRUE) {
  if (strict && variable(distribution) != "discrete") {
    stop("pmf only exists for discrete random variables; this distribution is ",
         variable(distribution), ". Perhaps you'd like to evaluate
         probabilities outside of strict mode?")
  }
  upper <- prob_left(distribution, of = at, inclusive = TRUE)
  lower <- prob_left(distribution, of = at, inclusive = FALSE)
  upper - lower
}

#' @export
realise.max <- function(distribution, n = 1) {
  d <- distribution$components$distributions
  draws <- distribution$components$draws
  iid_sample <- numeric(0L)
  for (i in seq_len(n)) {
    iid_sample_list <- Map(realise, d, draws)
    iid_sample[i] <- max(unlist(iid_sample_list))
  }
  iid_sample
}
