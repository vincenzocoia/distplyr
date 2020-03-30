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
	res <- list(components = list(distributions = dsts,
								  probs = probs))
	vvec <- vapply(dsts, variable, FUN.VALUE = character(1))
	v <- if (all(vvec == vvec[1])) vvec[1] else "mixed"
	new_dst(res, variable = v, class = "mix")
}

#' @export
get_mean.mix <- function(object, ...) {
	with(object[["components"]], {
		means <- vapply(distributions, get_mean, FUN.VALUE = numeric(1))
		sum(probs * means)
	})
}

#' @export
get_variance.mix <- function(object, ...) {
	with(object[["components"]], {
		means <- vapply(distributions, get_mean, FUN.VALUE = numeric(1))
		overall_mean <- sum(probs * means)
		variances <- vapply(distributions, get_variance, FUN.VALUE = numeric(1))
		sum(probs * (variances + means ^ 2 - overall_mean ^ 2))
	})
}

#' @export
get_probfn.mix <- function(object) {
	if (identical(variable(object), "mixed")) {
		return(NULL)
	}
	with(object[["components"]], {
		probfns <- lapply(distributions, get_probfn)
		function(x) {
			p_times_f <- mapply(function(p, f) p * f(x), probs, probfns,
								SIMPLIFY = FALSE)
			Reduce(`+`, p_times_f)
		}
	})
}

#' @export
get_cdf.mix <- function(object) {
	with(object[["components"]], {
		cdfs <- lapply(distributions, get_cdf)
		function(x) {
			p_times_cdfs <- mapply(function(p, f) p * f(x), probs, cdfs,
								   SIMPLIFY = FALSE)
			Reduce(`+`, p_times_cdfs)
		}
	})
}

#' @export
get_quantile.mix <- function(object) {
	stop("Not programmed yet.")
}

#' @export
get_randfn.mix <- function(object) {
	with(object[["components"]], {
		function(n) {
			if (n == 0) {
				if (identical(variable(object), "categorical")) {
					return(character())
				} else {
					return(numeric())
				}
			}
			randfns <- lapply(distributions, get_randfn)
			k <- length(distributions)
			id <- sample(1:k, size = n, replace = TRUE, prob = probs)
			sapply(id, function(i) randfns[[i]](1))
		}
	})
}
