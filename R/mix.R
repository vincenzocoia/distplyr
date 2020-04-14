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
	if (!identical(length(dsts), length(probs))) {
		stop("There must be one probability per distribution specified.")
	}
	if (any(probs < 0, na.rm = TRUE)) {
		stop("Probabilities must not be negative.")
	}
	if (sum(probs, na.rm = TRUE) != 1) {
		stop("Probabilities must sum to 1.")
	}
	na_probs <- is.na(probs)
	if (any(na_probs)) {
		warning("Found NA probabilities. Removing the corresponding distributions.")
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
	step_dfs <- lapply(dsts, discontinuities)
	y_vecs <- lapply(step_dfs, `[[`, "location")
	jump_vecs <- lapply(step_dfs, `[[`, "size")
	reduced_jumps <- mapply(`*`, probs, jump_vecs, SIMPLIFY = FALSE)
	jumps <- c(reduced_jumps, recursive = TRUE)
	y <- c(y_vecs, recursive = TRUE)
	new_steps <- aggregate_weights(y, jumps)
	v <- discontinuities_to_variable(new_steps)
	lgl_stepdst <- vapply(dsts, is_stepdst, FUN.VALUE = logical(1L))
	if (all(lgl_stepdst)) {
		return(stepdst(new_steps[["location"]],
					   weights = new_steps[["size"]],
					   variable = v))
	}
	res <- list(name = "Mixture",
				discontinuities = new_steps,
				components = list(distributions = dsts,
								  probs = probs))
	new_dst(res, variable = v, class = "mix")
}

#' @export
print.mix <- function(x, ...) {
	cat("Mixture Distribution\n")
	cat("\nComponents:\n")
	nm <- vapply(x[["components"]][["distributions"]], name,
				 FUN.VALUE = character(1L))
	df <- data.frame(distribution = nm, weight = x[["components"]][["probs"]])
	row.names(df) <- NULL
	print(df)
	cat("\nNumber of Discontinuities: ", nrow(discontinuities(x)))
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
get_quantile.mix <- function(object, tol = 1e-6, maxiter = 1000, ...) {
	distributions <- object[["components"]][["distributions"]]
	cdf <- get_cdf(object)
	discon <- discontinuities(object)
	function(x) {
		res <- x
		ones <- vapply(x == 1, isTRUE, FUN.VALUE = logical(1L))
		if (any(ones)) {
			right_ends <- lapply(distributions, eval_quantile, at = 1)
			res[ones] <- do.call(max, right_ends)
		}
		res[!ones] <- eval_quantile_from_cdf(
			cdf, discon, at = x[!ones], tol = tol, maxiter = maxiter
		)
		res
	}
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

#' @export
get_evi.mix <- function(object) {
	if (is_stepdst(object)) return(NaN)
	with(object[["components"]], {
		right_ends <- vapply(distributions, eval_quantile, at = 1, FUN.VALUE = numeric(1L))
		max_end <- max(right_ends)
		has_max_ends <- right_ends == max_end
		evis <- vapply(distributions, get_evi, FUN.VALUE = numeric(1L))
		final_sign <- if (max_end < Inf) -1 else 1
		final_sign * max(abs(evis[has_max_ends]))
	})
}
