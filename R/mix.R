#' Mixture Distributions
#'
#' Create a mixture distribution.
#'
#' @param ... Distribution objects to mix.
#' @param weights Vector of weights corresponding to the distributions;
#' or, single numeric for equal weights.
#' @param na.rm Remove distributions corresponding to \code{NA} weights?
#' Default is \code{FALSE}.
#' @return A mixture distribution -- an empty distribution if any weights
#' are \code{NA} and `na.rm = FALSE`, the default.
#' @examples
#' a <- dst_norm(0, 1)
#' b <- dst_norm(5, 2)
#' m1 <- mix(a, b, weights = c(1, 4))
#' plot(m1)
#' variable(m1)
#'
#' c <- dst_empirical(0:6)
#' m2 <- mix(a, b, c, weights = c(0.2, 0.5, 0.3))
#' plot(m2, n = 1001)
#' variable(m2)
#' @export
mix <- function(..., weights = 1, na.rm = FALSE) {
  dsts <- rlang::list2(...)
  if (!all(vapply(dsts, is_distribution, FUN.VALUE = logical(1L)))) {
    stop("Ellipsis must contain distributions only.")
  }
  n <- length(dsts)
  if (identical(length(weights), 1L)) {
    weights <- rep(weights, n)
  }
  if (!identical(n, length(weights))) {
    stop("There must be one weight per distribution specified.")
  }
  if (any(weights < 0, na.rm = TRUE)) {
    stop("Weights must not be negative.")
  }
  probs <- weights / sum(weights, na.rm = TRUE)
  na_probs <- is.na(probs)
  if (any(na_probs)) {
    if (!na.rm) {
      return(NA)
    }
    probs <- probs[!na_probs]
    dsts <- dsts[!na_probs]
  }
  zero_probs <- probs == 0
  if (any(zero_probs)) {
    probs <- probs[!zero_probs]
    dsts <- dsts[!zero_probs]
  }
  if (identical(length(probs), 1L)) {
    return(dsts[[1L]])
  }
  already_mixtures <- vapply(dsts, is_mix, FUN.VALUE = logical(1L))
  if (any(already_mixtures)) {
    dsts_mixture <- dsts[already_mixtures]
    dsts_inner <- lapply(dsts_mixture, function(dst) {
      dst$components$distributions
    })
    dsts_flat <- unlist(dsts_inner, recursive = FALSE)
    probs_mixture_outer <- probs[already_mixtures]
    probs_mixture_inner <- lapply(dsts_mixture, function(dst) {
      dst$components$probs
    })
    probs_mixture_list <- mapply(
      `*`,
      probs_mixture_outer,
      probs_mixture_inner,
      SIMPLIFY = FALSE
    )
    probs_mixture_flat <- unlist(probs_mixture_list, recursive = FALSE)
    stopifnot(length(probs_mixture_flat) == length(dsts_flat))
    new_probs <- c(probs_mixture_flat, probs[!already_mixtures])
    new_dsts <- c(dsts_flat, dsts[!already_mixtures])
    return(rlang::exec(mix, !!!new_dsts, weights = new_probs, na.rm = na.rm))
  }
  if (all(vapply(dsts, is_finite_dst, FUN.VALUE = logical(1L)))) {
    prob_dfs <- lapply(dsts, `[[`, "probabilities")
    y_list <- lapply(prob_dfs, `[[`, "location")
    y <- c(y_list, recursive = TRUE)
    weight_list_original <- lapply(prob_dfs, `[[`, "size")
    weight_list_scaled <- mapply(`*`, weight_list_original, probs,
      SIMPLIFY = FALSE
    )
    weight_scaled <- c(weight_list_scaled, recursive = TRUE)
    new_prob_df <- aggregate_weights(y, weight_scaled, sum_to_one = FALSE)
    res <- new_finite(list(probabilities = new_prob_df), variable = "discrete")
    return(res)
  }
  res <- list(components = list(
    distributions = dsts,
    probs = probs
  ))
  var_type <- vapply(dsts, variable, FUN.VALUE = character(1L))
  var_unique <- unique(var_type)
  if (length(var_unique) > 1L) {
    var_unique <- "mixed"
  }
  new_mix(res, variable = var_unique)
}

#' Constructor function for `mix` objects
#' @inheritParams new_distribution
new_mix <- function(l, variable, ..., class = character()) {
  new_distribution(l, variable = variable, class = c(class, "mix"))
}

#' @param object Object to be tested
#' @rdname mix
#' @export
is_mix <- function(object) inherits(object, "mix")

#' @rdname mix
#' @export
is.mix <- function(object) inherits(object, "mix")


#' @export
print.mix <- function(x, ...) {
  cat("Mixture Distribution\n")
  cat("\nComponents: ")
  if (requireNamespace("tibble", quietly = TRUE)) {
    cat("\n")
    print(tibble::as_tibble(x$components))
  } else {
    cat(length(x$components$probs))
  }
}

