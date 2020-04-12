#' @export
get_cdf.dst <- function(object) object$fun_cumu

#' @param tol tolerance
#' @param maxiter Maximum number of iterations
#' @rdname get_quantile
#' @export
get_quantile.dst <- function(object, tol = 1e-6, maxiter = 1000, ...) {
	f <- object[["representations"]][["fun_quant"]]
	if (!is.null(f)) return(f)
	function(x) eval_quantile_from_cdf(object, x, tol = tol, maxiter = maxiter)
}


#' @export
get_probfn.dst <- function(object) object$fun_prob


#' @export
get_randfn.dst <- function(object) {
	r <- object[["representations"]][["fun_rand"]]
	if (is.null(r)) {
		qf <- get_quantile(object)
		function(n) qf(stats::runif(n))
	} else {
		r
	}
}


#' @export
get_survival.dst <- function(object) {
	sf <- object[["representations"]][["fun_survival"]]
	if (is.null(sf)) {
		cdf <- get_cdf(object)
		function(x) 1 - cdf(x)
	} else {
		sf
	}
}


#' @export
get_chf.dst <- function(object) {
	if (variable(object) == "continuous") {
		sf <- get_survival(object)
		function(x) -log(sf(x))
	}
}



#' @export
get_hazard.dst <- function(object) {
	if (variable(object) == "continuous") {
		sf <- get_survival(object)
		pdf <- get_probfn(object)
		function(x) pdf(x) / sf(x)
	}
}
