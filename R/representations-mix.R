#' @export
eval_cdf.mix <- function(object, at) {
	with(object[["components"]], {
		cdf_vals <- lapply(distributions, eval_cdf, at = at)
		p_times_cdfs <- mapply(function(p, f) p * f, probs, cdf_vals,
							   SIMPLIFY = FALSE
		)
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
		cdf, discon,
		at = at[!ones], tol = tol, maxiter = maxiter
	)
	res
}

#' @export
eval_pmf.mix <- function(object, at, strict = TRUE, ...) {
	with(object$components, {
		cumulitive_sum <- 0
		for (i in 1:length(distributions)) {
			tryCatch(
				{
					cumlative_sum <- cumulitive_sum +
						eval_pmf(distributions[[i]], at, strict = strict) * probs[[i]]
				},
				error = function(c) {
					warning("A component distribution doesn't have a pmf. Perhaps you want to evaluate in non-strict mode?")
					return(NA)
				}
			)
		}
		cumulitive_sum
	})
}

#' @export
eval_density.mix <- function(object, at, strict = TRUE) {
	if (variable(object) != "continuous") {
		return(NULL)
	}
	with(object[["components"]], {
		density_vals <- lapply(distributions, eval_density, at = at)
		p_times_f <- mapply(function(p, f) p * f, probs, density_vals,
							SIMPLIFY = FALSE
		)
		Reduce(`+`, p_times_f)
	})
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
		vapply(id, function(i) realise(distributions[[i]]), FUN.VALUE = numeric(1L))
	})
}
