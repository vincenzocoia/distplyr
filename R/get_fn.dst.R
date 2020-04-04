#' @export
get_cdf.dst <- function(object) object$fun_cumu

#' @param low,high Lower and upper values to find all quantiles between.
#' Will improve later.
#' @param tol tolerance
#' @param maxiter Maximum number of iterations
#' @rdname get_quantile
#' @export
get_quantile.dst <- function(object, low, high, tol = 1e-6, maxiter = 1000, ...) {
	f <- object[["representations"]][["fun_quant"]]
	if (!is.null(f)) return(f)
	cdf <- get_cdf(object)
	maxiter <- 1000
	Vectorize(function(x) {
		if (cdf(low) >= x) {
			stop("cdf at low value must evaluate to <p.")
		}
		if (cdf(high) < x) {
			stop("cdf at high value must evaluate to >=p.")
		}
		w <- high - low
		i <- 0
		while(w > tol && i <= maxiter) {
			i <- i + 1
			mid <- (high + low) / 2
			val <- cdf(mid)
			if (val >= x) {
				high <- mid
			} else {
				low <- mid
			}
			if (high == low) return(low)
			cat(low, high, "\n")
		}
		if (i == maxiter) {
			warning("Maximum number of iterations reached before tolerance was achieved.")
		}
		mid
	})

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
