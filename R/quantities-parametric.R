#' @export
mean.parametric <- function(x) {
	quantity_parametric(x, "mean")
}

#' @export
variance.parametric <- function(x) {
	quantity_parametric(x, "variance")
}

#' @export
skewness.parametric <- function(x) {
	quantity_parametric(x, "skewness")
}

#' @export
kurtosis_exc.parametric <- function(x) {
	quantity_parametric(x, "kurtosis_exc")
}

#' @export
evi.parametric <- function(x) {
	quantity_parametric(x, "evi")
}

#' @export
range.parametric <- function(x) {
	quantity_parametric(x, "range")
}

#' @export
median.parametric <- function(x) {
	quantity_parametric(x, "median")
}

quantity_parametric <- function(distribution, quantity) {
	d_name <- distribution$name
	q_expr <- quantities[[d_name]][[quantity]]
	if (!is.null(q_expr)) {
		rlang::eval_tidy(q_expr, data = parameters(distribution))
	} else {
		rlang::exec("NextMethod", .env = rlang::caller_env(n = 1))
	}
}
