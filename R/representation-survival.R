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
#' \code{\link{realise}}
#' @export
eval_survival <- function(object, at) UseMethod("eval_survival")

#' Survival Function
#'
#' @param object Object of class "dst"
#' @return a vectorized function of the survival.
#' @seealso
#' \code{\link{get_quantile}},
#' \code{\link{get_probfn}},
#' \code{\link{get_cdf}},
#' \code{\link{get_hazard}},
#' \code{\link{realise}}
#' @export
get_survival <- function(object) UseMethod("get_survival")


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
eval_survival.dst <- function(object, at) {
	get_survival(object)(at)
}
