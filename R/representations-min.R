#' @export
prob_right.min <- function(distribution, of, inclusive) {
  d <- distribution$components$distributions
  draws <- distribution$components$draws
  prob_rights <- lapply(d, prob_right, of = of, inclusive = inclusive)
  contributions <- Map(`^`, prob_rights, draws)
  Reduce(`*`, contributions)
}

#' @export
eval_survival.min <- function(distribution, at) {
  prob_right(distribution, of = at, inclusive = FALSE)
}

#' @export
eval_cdf.min <- function(distribution, at) {
  1 - prob_right(distribution, of = at, inclusive = FALSE)
}

#' @export
eval_density.min <- function(distribution, at, strict = TRUE) {
  # formula: survival * (sum draws_j f_j / surv_j)
  d <- distribution$components$distributions
  draws <- distribution$components$draws
  full_surv <- eval_survival(distribution, at = at)
  survs <- lapply(d, eval_survival, at = at)
  pdfs <- lapply(d, eval_density, at = at, strict = strict)
  divide_if_nonzero <- function(draws, pdf, cdf) {
    res <- draws * pdf / cdf
    res[pdf == 0] <- 0
    res
  }
  ratios <- Map(divide_if_nonzero, draws, pdfs, survs)
  ratios_sum <- Reduce(`+`, ratios)
  ratios_sum * full_surv
}

#' @export
eval_pmf.min <- function(distribution, at, strict = TRUE) {
  if (strict && variable(distribution) != "discrete") {
    stop("pmf only exists for discrete random variables; this distribution is ",
         variable(distribution), ". Perhaps you'd like to evaluate
         probabilities outside of strict mode?")
  }
  upper <- prob_right(distribution, of = at, inclusive = TRUE)
  lower <- prob_right(distribution, of = at, inclusive = FALSE)
  upper - lower
}

#' @export
realise.min <- function(distribution, n = 1) {
  d <- distribution$components$distributions
  draws <- distribution$components$draws
  iid_sample <- numeric(0L)
  for (i in seq_len(n)) {
    iid_sample_list <- Map(realise, d, draws)
    iid_sample[i] <- min(unlist(iid_sample_list))
  }
  iid_sample
}
