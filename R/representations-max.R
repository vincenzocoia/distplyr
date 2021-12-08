#' @export
prob_left.max <- function(distribution, of, inclusive) {
  d <- distribution$components$distributions
  prob_lefts <- lapply(d, prob_left, of = of, inclusive = inclusive)
  Reduce(`*`, prob_lefts)
}

#' @export
eval_cdf.max <- function(distribution, at) {
  prob_left(distribution, of = at, inclusive = TRUE)
}

#' @export
eval_density.max <- function(distribution, at) {
  # formula: cdf * (sum f_j / F_j)
  d <- distribution$components$distributions
  full_cdf <- eval_cdf(distribution, at = at)
  cdfs <- lapply(d, eval_cdf, at = at)
  pdfs <- lapply(d, eval_density, at = at)
  divide_if_nonzero <- function(pdf, cdf) {
    ratio <- pdf / cdf
    ratio[pdf == 0] <- 0
    ratio
  }
  ratios <- Map(divide_if_nonzero, pdfs, cdfs)
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
  draws <- vapply(d, realise, FUN.VALUE = numeric(1L))
  max(draws)
}
