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
	distionary::dst_empirical(df$location_expr, weights = df$size)
}

#' Ops for a Finite Distribution
#'
#' @param e1,e2 Objects on the left- and right-hand sides of a binary
#' operator, or in the case of a unary operator, `e1` is the only argument.
#' @param distribution Distribution object to transform.
#' @details Although the default `Ops` for distributions is defined as a wrapper
#' around the verbs `shift()`, `multiply()`, `flip()`, and `invert()`,
#' the opposite is true for `"finite"` distributions. This is so that
#' `Ops.finite()` can piggyback off of the `Ops` defined on the vector
#' of finite observations.
#' @return A transformed distribution. This turns out to be another
#' finite distribution, with the original outcomes transformed
#' by the specified `Ops`.
#' @rdname ops_finite
#' @export
Ops.finite <- function(e1, e2) {
	op <- .Generic[[1]]
	if (distionary::is_distribution(e1)) {
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

#' @rdname ops_finite
#' @export
invert.finite <- function(distribution) {
	1 / distribution
}

#' @rdname ops_finite
#' @export
flip.finite <- function(distribution) {
	-distribution
}

#' @rdname ops_finite
#' @export
shift.finite <- function(distribution, constant) {
	distribution + constant
}

#' @rdname ops_finite
#' @export
multiply.finite <- function(distribution, constant) {
	distribution * constant
}
