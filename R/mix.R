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
#' c <- stepdst(0:6)
#' m2 <- mix(a, b, c, weights = c(0.2, 0.5, 0.3))
#' plot(m2, n = 1001)
#' variable(m2)
#' @export
mix <- function(..., weights = 1, na.rm = FALSE) {
	dsts <- rlang::list2(...)
	lapply(dsts, function(.dst) if (!is_distribution(.dst)) {
		stop("Ellipses must contain distributions only.")
	})
	n <- length(dsts)
	if (identical(length(weights), 1L)) {
		weights <- rep(weights, n)
	}
	if (!identical(n, length(weights))) {
		stop("There must be one probability per distribution specified.")
	}
	if (any(weights < 0, na.rm = TRUE)) {
		stop("Weights must not be negative.")
	}
	probs <- weights / sum(weights, na.rm = TRUE)
	na_probs <- is.na(probs)
	if (any(na_probs)) {
		if (!na.rm) return(distribution())
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
		l <- list(name = "Mixture", discontinuities = new_steps)
		res <- new_stepdst(l, variable = "discrete")
		class(res) <- c("stepdst", "mix", "dst")  # Hacky and temporary
		return(res)
	}
	res <- list(name = "Mixture",
				discontinuities = new_steps,
				components = list(distributions = dsts,
								  probs = probs))
	new_distribution(res, variable = v, class = "mix")
}

#' @export
print.mix <- function(x, ...) {
	cat("Mixture Distribution\n")
	cat("\nComponents:\n")
	nm <- vapply(x[["components"]][["distributions"]], name,
				 FUN.VALUE = character(1L))
	df <- data.frame(distribution = nm, weight = x[["components"]][["probs"]])
	if (requireNamespace("tibble", quietly = TRUE)) {
		df <- tibble::as_tibble(df)
	} else {
		row.names(df) <- NULL
	}
	print(df)
	cat("\nNumber of Discontinuities: ", nrow(discontinuities(x)))
}

#' @export
mean.mix <- function(object, ...) {
	with(object[["components"]], {
		means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
		sum(probs * means)
	})
}

#' @export
variance.mix <- function(object, ...) {
	overall_mean <- mean(object)
	with(object[["components"]], {
		means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
		variances <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
		sum(probs * (variances + means ^ 2 - overall_mean ^ 2))
	})
}

#' @export
skewness.mix <- function(object, ...) {
	overall_mean <- mean(object)
	overall_sd <- sd(object)
	with(object[["components"]], {
		means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
		vars <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
		sds <- sqrt(vars)
		skews <- vapply(distributions, skewness, FUN.VALUE = numeric(1L))
		cmoms <- list(zero = 1,
					  first = 0,
					  second = vars,
					  third = skews * sds ^ 3)
		terms <- lapply(0:3, function(k) {
			choose(3, k) * (means - overall_mean) ^ (3 - k) * cmoms[[k + 1L]]
		})
		sum(probs * Reduce(`+`, terms)) / overall_sd ^ 3
	})
}

#' @export
kurtosis_exc.mix <- function(object, ...) {
	overall_mean <- mean(object)
	overall_var <- variance(object)
	with(object[["components"]], {
		means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
		vars <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
		sds <- sqrt(vars)
		skews <- vapply(distributions, skewness, FUN.VALUE = numeric(1L))
		kurts <- vapply(distributions, kurtosis_raw, FUN.VALUE = numeric(1L))
		cmoms <- list(zero = 1,
					  first = 0,
					  second = vars,
					  third = skews * sds ^ 3,
					  fourth = vars ^ 2 * kurts)
		terms <- lapply(0:4, function(k) {
			choose(4, k) * (means - overall_mean) ^ (4 - k) * cmoms[[k + 1L]]
		})
		sum(probs * Reduce(`+`, terms)) / overall_var ^ 2 - 3
	})
}

#' @export
eval_density.mix <- function(object, at) {
	if (variable(object) != "continuous") {
		return(NULL)
	}
	with(object[["components"]], {
		density_vals <- lapply(distributions, eval_density, at = at)
		p_times_f <- mapply(function(p, f) p * f, probs, density_vals,
							SIMPLIFY = FALSE)
		Reduce(`+`, p_times_f)
	})
}

#' @export
eval_cdf.mix <- function(object, at) {
	with(object[["components"]], {
		cdf_vals <- lapply(distributions, eval_cdf, at = at)
		p_times_cdfs <- mapply(function(p, f) p * f, probs, cdf_vals,
							   SIMPLIFY = FALSE)
		Reduce(`+`, p_times_cdfs)
	})
}

#' @export
eval_quantile.mix <- function(object, at, tol = 1e-6, maxiter = 1000, ...) {
	distributions <- object[["components"]][["distributions"]]
	cdf <- get_cdf(object)
	discon <- discontinuities(object)
	res <- at
	ones <- vapply(at == 1, isTRUE, FUN.VALUE = logical(1L))
	if (any(ones)) {
		right_ends <- lapply(distributions, eval_quantile, at = 1)
		res[ones] <- do.call(max, right_ends)
	}
	res[!ones] <- eval_quantile_from_cdf(
		cdf, discon, at = at[!ones], tol = tol, maxiter = maxiter
	)
	res
}

#' @export
realise.mix <- function(object, n = 1, ...) {
	with(object[["components"]], {
		if (n == 0) {
			if (identical(variable(object), "categorical")) {
				return(character())
			} else {
				return(numeric())
			}
		}
		k <- length(distributions)
		id <- sample(1:k, size = n, replace = TRUE, prob = probs)
		sapply(id, function(i) realise(distributions[[i]]))
	})
}

#' @export
evi.mix <- function(object) {
	if (is_stepdst(object)) return(NaN)
	with(object[["components"]], {
		right_ends <- vapply(distributions, eval_quantile, at = 1, FUN.VALUE = numeric(1L))
		max_end <- max(right_ends)
		has_max_ends <- right_ends == max_end
		evis <- vapply(distributions, evi, FUN.VALUE = numeric(1L))
		final_sign <- if (max_end < Inf) -1 else 1
		final_sign * max(abs(evis[has_max_ends]))
	})
}
