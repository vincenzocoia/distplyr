#' Mixture Distributions
#'
#' Create a mixture distribution.
#'
#' @param ... Distribution objects to mix.
#' @param probs Vector of probabilities corresponding to the distributions.
#' @return A mixture distribution.
#' @export
mix <- function(..., probs) {
	dsts <- list(...)
	lapply(dsts, function(.dst) if (!is_dst(.dst)) {
		stop("Elipses must contain distributions only.")
	})
	n <- length(dsts)
	if (n != length(probs)) {
		stop("There must be one probability per distribution specified.")
	}
	if (any(probs < 0)) {
		stop("Probabilities must not be negative.")
	}
	if (sum(probs) != 1) {
		stop("Probabilities must sum to 1.")
	}
	na_probs <- is.na(probs)
	if (any(na_probs)) {
		warning("Found NA probabilities. Removing the corresponding distribution.")
		probs <- probs[!na_probs]
		dsts <- dsts[!na_probs]
	}
	zero_probs <- probs == 0
	if (any(zero_probs)) {
		probs <- probs[!zero_probs]
		dsts <- dsts[!zero_probs]
	}
	n <- length(probs)
	if (n == 1) {
		return(dsts[[1]])
	}
	res <- list(distributions = dsts,
				probs = probs)
	new_dst(res, class = "mix")
}

#' @export
get_mean.mix <- function(object, ...) {
	dsts <- object[["distributions"]]
	p <- object[["probs"]]
	means <- vapply(dsts, get_mean, numeric(1))
	sum(p * means)
}

#' @export
get_variance.mix <- function(object, ...) {
	dsts <- object[["distributions"]]
	p <- object[["probs"]]
	means <- vapply(dsts, get_mean, numeric(1))
	vars <- vapply(dsts, get_variance, numeric(1))
	mean <- get_mean(object)
	sum(p * (vars + means ^ 2 - mean ^ 2))
}
