#' Mutate Parameters
#'
#' Modify distribution parameters. A lightweight function that only
#' checks that the parameter you're modifying already exists in the
#' distribution.
#' @param distribution Parametric distribution
#' @param ... Named expressions; quoted. Names should be parameters names of the
#' distribution. Expressions can involve computations with other parameters.
#' @return The input distribution, with the parameters modified as specified
#' in `...`.
#' @examples
#' d <- dst_unif(1, 3)
#' distplyr:::mutate_parameters(d, min = max - min)
mutate_parameters <- function(distribution, ...) {
	ell <- rlang::enquos(...)
	params <- parameters(distribution)
	param_names <- names(params)
	for (i in seq_along(ell)) {
		param_name <- names(ell)[[i]]
		stopifnot(param_name %in% param_names)
		param_val <- ell[[i]]
		new_val <- rlang::eval_tidy(param_val, data = params)
		distribution$parameters[[param_name]] <- new_val
	}
	distribution
}

#' Mutate Discrete Values
#'
#' Transform discrete values in a finite distribution.
#' @param distribution Finite distribution.
#' @param location An expression involving `location`.
#' @return The input distribution with discrete values modified according
#' to the expression in `location`.
mutate_finite <- function(distribution, location) {
	l <- rlang::enquo(location)
	df <- distribution$probabilities
	df$location <- rlang::eval_tidy(l, data = df)
	dst_empirical(df$location, weights = df$size)
}
