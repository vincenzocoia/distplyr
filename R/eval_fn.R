#' Evaluate cdf
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the cdf at
#' @return Vector of cdf values
#' @seealso
#' \code{\link{eval_quantfn}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazfn}},
#' \code{\link{eval_surv}}
#' @export
eval_cdf <- function(object, at) UseMethod("eval_cdf")

#' @export
eval_cdf.dst <- function(object, at) {
	cdf <- object$fun_cumu
	cdf(at)
}

#' Evaluate quantile function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the quantile function at
#' @return Vector of quantiles
#' @seealso
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazfn}},
#' \code{\link{eval_surv}}
#' @export
eval_quantfn <- function(object, at) UseMethod("eval_quantfn")

#' @export
eval_quantfn.dst <- function(object, at) {
	qf <- object$fun_quant
	qf(at)
}

#' Evaluate probability density/mass function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the density or mass function at
#' @return Vector of density/mass values
#' @seealso
#' \code{\link{eval_quantfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazfn}},
#' \code{\link{eval_surv}}
#' @export
eval_probfn <- function(object, at) UseMethod("eval_probfn")

#' @export
eval_probfn.dst <- function(object, at) {
	f <- object$fun_prob
	f(at)
}

#' Generate random sample from a distribution
#'
#' @param object Object of class "dst"
#' @param at Number of observations to generate
#' @return Vector of independent values drawn from the inputted distribution
#' @seealso
#' \code{\link{eval_quantfn}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_hazfn}},
#' \code{\link{eval_surv}}
#' @export
eval_randfn <- function(object, at) UseMethod("eval_randfn")

#' @export
eval_randfn.dst <- function(object, at) {
	f <- object$fun_rand
	f(at)
}


#' Evaluate Hazard Function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the hazard function at.
#' @return Vector of the evaluated hazard function
#' @seealso
#' \code{\link{eval_quantfn}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_surv}}
#' @export
eval_hazfn <- function(object, at) UseMethod("eval_hazfn")

#' @export
eval_hazfn.dst <- function(object, at) {
	f <- object$fun_haz
	f(at)
}


#' Evaluate Survival Function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the survival function at.
#' @return Vector of the evaluated survival function
#' @seealso
#' \code{\link{eval_quantfn}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_hazfn}},
#' \code{\link{eval_randfn}}
#' @export
eval_surv <- function(object, at) UseMethod("eval_surv")

#' @export
eval_surv.dst <- function(object, at) {
	f <- object$fun_surv
	f(at)
}
