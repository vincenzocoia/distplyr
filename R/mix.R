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
	variables <- vapply(dsts, variable, FUN.VALUE = character(1L))
	if (all(variables == variables[1L])) {
		v <- variables[1L]
	} else {
		v <- "mixed"
	}
	step_dfs <- lapply(dsts, steps)
	y_vecs <- lapply(step_dfs, `[[`, "y")
	jump_vecs <- lapply(step_dfs, `[[`, "prob")
	reduced_jumps <- mapply(`*`, probs, jump_vecs, SIMPLIFY = FALSE)
	jumps <- c(jump_vecs, recursive = TRUE)
	y <- c(y_vecs, recursive = TRUE)
	new_steps <- make_steps(y, jumps)
	lgl_stepdst <- vapply(dsts, is_stepdst, FUN.VALUE = logical(1L))
	if (all(lgl_stepdst)) {
		return(stepdst(y, data = new_steps, weights = prob, variable = v))
	}
	cdf_at_y <- lapply(dsts, eval_cdf, at = y)
	level_components <- mapply(`*`, cdf_at_y, probs, SIMPLIFY = FALSE)
	new_steps[["tau"]] <- Reduce(`+`, level_components)
	res <- list(steps = new_steps,
				components = list(distributions = dsts,
								  probs = probs))
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
