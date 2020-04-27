#' Generate a Sample from a Distribution
#'
#' @param object Object of class "dst"
#' @param n Number of observations to generate
#' @return Vector of independent values drawn from the inputted distribution
#' @rdname realise
#' @export
realise <- function(object, n = 1) UseMethod("realise")

#' @rdname realise
#' @export
realize <- function(object, n = 1) UseMethod("realise")

#' @export
realise.dst <- function(object, n) {
	r <- object[["representations"]][["fun_rand"]]
	if (is.null(r)) {
		u <- stats::runif(n)
		return(eval_quantile(object, at = u))
	} else {
		r
	}
}
