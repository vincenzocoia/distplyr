#' Mutate Discrete Values
#'
#' Transform discrete values in a finite distribution.
#'
#' @param distribution Finite distribution.
#' @param location_expr An expression involving `location`.
#' @return The input distribution with discrete values modified according
#' to the expression in `location`.
mutate_finite <- function(distribution, location_expr) {
	l <- rlang::enquo(location_expr)
	df <- distribution$probabilities
	df$location_expr <- rlang::eval_tidy(l, data = df)
	dst_empirical(df$location_expr, weights = df$size)
}

#' @export
Ops.finite <- function(e1, e2) {
	op <- .Generic[[1]]
	if (is_distribution(e1)) {
		if (missing(e2)) {
			call <- rlang::call2(op, expr(location))
		} else {
			call <- rlang::call2(op, expr(location), e2)
		}
		mutate_finite(e1, !!call)
	} else {
		call <- rlang::call2(op, e1, expr(location))
		mutate_finite(e2, !!call)
	}
}

#' @export
invert.finite <- function(distribution) {
	1 / distribution
}

#' @export
flip.finite <- function(distribution) {
	-distribution
}

#' @export
shift.finite <- function(distribution, constant) {
	distribution + constant
}

#' @export
multiply.finite <- function(distribution, constant) {
	distribution * constant
}
