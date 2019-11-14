#' Calculate cdf
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the cdf at
#' @return Vector of cdf values if \code{at} is present; otherwise,
#' returns a function for the cdf.
#' @seealso
#' \code{\link{fun_quant}},
#' \code{\link{fun_prob}},
#' \code{\link{fun_rand}},
#' \code{\link{fun_haz}},
#' \code{\link{fun_surv}}
#' @export
fun_cumu <- function(object, at) UseMethod("fun_cumu")

#' @export
fun_cumu.dst <- function(object, at) {
	cdf <- object$fun_cumu
	if (missing(at)) return(cdf)
	cdf(at)
}

#' Calculate quantile function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the quantile function at
#' @return Vector of quantiles if \code{at} is present; otherwise,
#' returns a function for the quantile function.
#' @seealso
#' \code{\link{fun_prob}},
#' \code{\link{fun_cumu}},
#' \code{\link{fun_rand}},
#' \code{\link{fun_haz}},
#' \code{\link{fun_surv}}
#' @export
fun_quant <- function(object, at) UseMethod("fun_quant")

#' @export
fun_quant.dst <- function(object, at) {
	qf <- object$fun_quant
	if (missing(at)) return(qf)
	qf(at)
}

#' Calculate density/mass function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the density or mass function at
#' @return Vector of density/mass values if \code{at} is present; otherwise,
#' returns a function for the pdf/pmf.
#' @seealso
#' \code{\link{fun_quant}},
#' \code{\link{fun_cumu}},
#' \code{\link{fun_rand}},
#' \code{\link{fun_haz}},
#' \code{\link{fun_surv}}
#' @export
fun_prob <- function(object, at) UseMethod("fun_prob")

#' @export
fun_prob.dst <- function(object, at) {
	f <- object$fun_prob
	if (missing(at)) return(f)
	f(at)
}

#' Generate random sample from a distribution
#'
#' @param object Object of class "dst"
#' @param at Number of observations to generate
#' @return Vector of independent values drawn from the inputted distribution
#' if \code{at} is present; otherwise,
#' returns a function for a random number generator.
#' @seealso
#' \code{\link{fun_quant}},
#' \code{\link{fun_prob}},
#' \code{\link{fun_cumu}},
#' \code{\link{fun_haz}},
#' \code{\link{fun_surv}}
#' @export
fun_rand <- function(object, at) UseMethod("fun_rand")

#' @export
fun_rand.dst <- function(object, at) {
	f <- object$fun_rand
	if (missing(at)) return(f)
	f(at)
}


#' Calculate Hazard Function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the hazard function at (optional).
#' @return Vector of the evaluated hazard function
#' if \code{at} is present; otherwise,
#' returns a function for the hazard.
#' @seealso
#' \code{\link{fun_quant}},
#' \code{\link{fun_prob}},
#' \code{\link{fun_cumu}},
#' \code{\link{fun_rand}},
#' \code{\link{fun_surv}}
#' @export
fun_haz <- function(object, at) UseMethod("fun_haz")

#' @export
fun_haz.dst <- function(object, at) {
	f <- object$fun_haz
	if (missing(at)) return(f)
	f(at)
}


#' Calculate Survival Function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the survival function at (optional).
#' @return Vector of the evaluated survival function
#' if \code{at} is present; otherwise,
#' returns a function for the survival.
#' @seealso
#' \code{\link{fun_quant}},
#' \code{\link{fun_prob}},
#' \code{\link{fun_cumu}},
#' \code{\link{fun_haz}},
#' \code{\link{fun_rand}}
#' @export
fun_surv <- function(object, at) UseMethod("fun_surv")

#' @export
fun_surv.dst <- function(object, at) {
	f <- object$fun_surv
	if (missing(at)) return(f)
	f(at)
}
