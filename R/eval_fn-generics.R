#' Evaluate cdf
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the cdf at
#' @return Vector of cdf values
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @export
eval_cdf <- function(object, at) UseMethod("eval_cdf")


#' Evaluate quantile function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the quantile function at
#' @return Vector of quantiles
#' @seealso
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @export
eval_quantile <- function(object, at) UseMethod("eval_quantile")



#' Evaluate probability density/mass function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the density or mass function at
#' @return Vector of density/mass values
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @export
eval_probfn <- function(object, at) UseMethod("eval_probfn")

#' Generate random sample from a distribution
#'
#' @param object Object of class "dst"
#' @param at Number of observations to generate
#' @return Vector of independent values drawn from the inputted distribution
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @export
eval_randfn <- function(object, at) UseMethod("eval_randfn")


#' Evaluate Hazard Function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the hazard function at.
#' @return Vector of the evaluated hazard function
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_randfn}},
#' \code{\link{eval_survival}}
#' @export
eval_hazard <- function(object, at) UseMethod("eval_hazard")


#' Evaluate Survival Function
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the survival function at.
#' @return Vector of the evaluated survival function
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{eval_cdf}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_randfn}}
#' @export
eval_survival <- function(object, at) UseMethod("eval_survival")
