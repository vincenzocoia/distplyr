#' Get cdf
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the cdf.
#' @seealso
#' \code{\link{get_quantfn}},
#' \code{\link{get_probfn}},
#' \code{\link{get_randfn}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_surv}}
#' @export
get_cdf <- function(object) UseMethod("get_cdf")

#' @export
get_cdf.dst <- function(object) object$fun_cumu

#' Get quantile function
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the quantile function.
#' @seealso
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_randfn}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_surv}}
#' @export
get_quantfn <- function(object) UseMethod("get_quantfn")

#' @export
get_quantfn.dst <- function(object) object$fun_quant

#' Get probability density/mass function
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the pdf/pmf.
#' @seealso
#' \code{\link{get_quantfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_randfn}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_surv}}
#' @export
get_probfn <- function(object) UseMethod("get_probfn")

#' @export
get_probfn.dst <- function(object) object$fun_prob

#' Get random sampler function from a distribution
#'
#' @param object Object of class "dst"
#' @return a function accepting an integer number of values to generate.
#' @seealso
#' \code{\link{get_quantfn}},
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_surv}}
#' @export
get_randfn <- function(object) UseMethod("get_randfn")

#' @export
get_randfn.dst <- function(object) object$fun_rand


#' Get Hazard Function
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the hazard.
#' @seealso
#' \code{\link{get_quantfn}},
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_randfn}},
#' \code{\link{get_surv}}
#' @export
get_hazfn <- function(object) UseMethod("get_hazfn")

#' @export
get_hazfn.dst <- function(object) object$fun_haz


#' Calculate Survival Function
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the survival.
#' @seealso
#' \code{\link{get_quantfn}},
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_randfn}}
#' @export
get_surv <- function(object) UseMethod("get_surv")

#' @export
get_surv.dst <- function(object) object$fun_surv
