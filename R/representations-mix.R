#' @export
eval_cdf.mix <- function(distribution, at) {
	with(distribution[["components"]], {
		cdf_vals <- lapply(distributions, distionary::eval_cdf, at = at)
		p_times_cdfs <- mapply(`*`, probs, cdf_vals, SIMPLIFY = FALSE)
		Reduce(`+`, p_times_cdfs)
	})
}

#' @export
eval_pmf.mix <- function(distribution, at, strict = TRUE, ...) {
	with(distribution[["components"]], {
		pmf_vals <- lapply(
			distributions, distionary::eval_pmf, at = at, strict = strict
		)
		p_times_f <- mapply(`*`, probs, pmf_vals, SIMPLIFY = FALSE)
		Reduce(`+`, p_times_f)
	})
}

#' @export
eval_density.mix <- function(distribution, at, strict = TRUE) {
	with(distribution[["components"]], {
		density_vals <- lapply(
			distributions, distionary::eval_density, at = at, strict = strict
		)
		p_times_f <- mapply(`*`, probs, density_vals, SIMPLIFY = FALSE)
		Reduce(`+`, p_times_f)
	})
}

#' @export
realise.mix <- function(distribution, n = 1, ...) {
	with(distribution[["components"]], {
		if (n == 0) {
			return(numeric())
		}
		k <- length(distributions)
		id <- sample(1:k, size = n, replace = TRUE, prob = probs)
		vapply(id, function(i) distionary::realise(distributions[[i]], n = 1),
			   FUN.VALUE = numeric(1L))
	})
}
