#' @export
mean.mix <- function(x, ...) {
	with(x[["components"]], {
		means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
		sum(probs * means)
	})
}

#' @export
variance.mix <- function(x, ...) {
	overall_mean <- mean(x)
	with(x[["components"]], {
		means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
		variances <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
		sum(probs * (variances + means^2 - overall_mean^2))
	})
}

#' @export
skewness.mix <- function(x, ...) {
	overall_mean <- mean(x)
	overall_sd <- stdev(x)
	with(x[["components"]], {
		means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
		vars <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
		sds <- sqrt(vars)
		skews <- vapply(distributions, skewness, FUN.VALUE = numeric(1L))
		cmoms <- list(
			zero = 1,
			first = 0,
			second = vars,
			third = skews * sds^3
		)
		terms <- lapply(0:3, function(k) {
			choose(3, k) * (means - overall_mean)^(3 - k) * cmoms[[k + 1L]]
		})
		sum(probs * Reduce(`+`, terms)) / overall_sd^3
	})
}

#' @export
kurtosis_exc.mix <- function(x, ...) {
	overall_mean <- mean(x)
	overall_var <- variance(x)
	with(x[["components"]], {
		means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
		vars <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
		sds <- sqrt(vars)
		skews <- vapply(distributions, skewness, FUN.VALUE = numeric(1L))
		kurts <- vapply(distributions, kurtosis_raw, FUN.VALUE = numeric(1L))
		cmoms <- list(
			zero = 1,
			first = 0,
			second = vars,
			third = skews * sds^3,
			fourth = vars^2 * kurts
		)
		terms <- lapply(0:4, function(k) {
			choose(4, k) * (means - overall_mean)^(4 - k) * cmoms[[k + 1L]]
		})
		sum(probs * Reduce(`+`, terms)) / overall_var^2 - 3
	})
}

#' @export
evi.mix <- function(x, ...) {
	if (is_finite_dst(x)) {
		return(NaN)
	}
	with(x[["components"]], {
		right_ends <- vapply(distributions, eval_quantile,
							 at = 1,
							 FUN.VALUE = numeric(1L)
		)
		max_end <- max(right_ends)
		has_max_ends <- right_ends == max_end
		evis <- vapply(distributions, evi, FUN.VALUE = numeric(1L))
		final_sign <- if (max_end < Inf) -1 else 1
		final_sign * max(abs(evis[has_max_ends]))
	})
}


#' @export
range.mix <- function(distribution, ...) {
	r <- lapply(distribution$components$distributions, range)
	low <- Reduce(min, r)
	high <- Reduce(max, r)
	c(low, high)
}
