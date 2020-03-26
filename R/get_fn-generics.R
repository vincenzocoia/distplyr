#' Get cdf
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the cdf
#' @seealso
#' \code{\link{get_quantile}},
#' \code{\link{get_probfn}},
#' \code{\link{get_randfn}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_surv}}
#' @export
get_cdf <- function(object) UseMethod("get_cdf")



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
get_quantile <- function(object) UseMethod("get_quantile")


#' Get probability density/mass function
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the pdf/pmf.
#' @seealso
#' \code{\link{get_quantile}},
#' \code{\link{get_cdf}},
#' \code{\link{get_randfn}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_surv}}
#' @export
get_probfn <- function(object) UseMethod("get_probfn")


#' Random sampler function from a distribution
#'
#' @param object Object of class "dst"
#' @return a function accepting an integer number of values to generate.
#' @seealso
#' \code{\link{get_quantile}},
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_surv}}
#' @export
get_randfn <- function(object) UseMethod("get_randfn")




#' Survival Function
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the survival.
#' @seealso
#' \code{\link{get_quantile}},
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_hazfn}},
#' \code{\link{get_randfn}}
#' @export
get_surv <- function(object) UseMethod("get_surv")



#' Hazard Function
#'
#' Hazard and cumulative hazard functions.
#' @param object Distribution object
#' @rdname hazard
#' @export
get_chf <- function(object) UseMethod("get_chf")

#' @rdname hazard
#' @export
get_hazard <- function(object) UseMethod("get_hazard")




