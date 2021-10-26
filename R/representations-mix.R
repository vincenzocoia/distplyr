#' @export
eval_cdf.mix <- function(distribution, at) {
	with(distribution[["components"]], {
		cdf_vals <- lapply(distributions, eval_cdf, at = at)
		p_times_cdfs <- mapply(function(p, f) p * f, probs, cdf_vals,
							   SIMPLIFY = FALSE
		)
		Reduce(`+`, p_times_cdfs)
	})
}

#' @export
eval_quantile.mix <- function(distribution, at, tol = 1e-6, maxiter = 1000, ...) {
	distributions <- distribution[["components"]][["distributions"]]
	cdf <- get_cdf(distribution)
	discon <- discontinuities(distribution)
	res <- at
	ones <- vapply(at == 1, isTRUE, FUN.VALUE = logical(1L))
	if (any(ones)) {
		right_ends <- lapply(distributions, eval_quantile, at = 1)
		res[ones] <- do.call(max, right_ends)
	}
	res[!ones] <- eval_quantile_from_cdf(
		cdf, discon,
		at = at[!ones], tol = tol, maxiter = maxiter
	)
	res
}

#' @export
eval_pmf.mix <- function(distribution, at, strict = TRUE, ...) {
	with(distribution[["components"]], {
		pmf_vals <- lapply(distributions, eval_pmf, at = at, strict = strict)
		p_times_f <- mapply(`*`, probs, pmf_vals, SIMPLIFY = FALSE)
		Reduce(`+`, p_times_f)
	})
}

#' @export
eval_density.mix <- function(distribution, at, strict = TRUE) {
	if (variable(distribution) != "continuous") {
		return(NULL)
	}
	with(distribution[["components"]], {
		density_vals <- lapply(distributions, eval_density, at = at)
		p_times_f <- mapply(function(p, f) p * f, probs, density_vals,
							SIMPLIFY = FALSE)
		Reduce(`+`, p_times_f)
	})
}

#' @export
realise.mix <- function(distribution, n = 1, ...) {
	with(distribution[["components"]], {
		if (n == 0) {
			if (identical(variable(distribution), "categorical")) {
				return(character())
			} else {
				return(numeric())
			}
		}
		k <- length(distributions)
		id <- sample(1:k, size = n, replace = TRUE, prob = probs)
		vapply(id, function(i) realise(distributions[[i]]), FUN.VALUE = numeric(1L))
	})
}
