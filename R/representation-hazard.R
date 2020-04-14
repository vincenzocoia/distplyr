#' Evaluate Hazard Function
#'
#' Evaluate hazard function or cumulative
#' hazard function (chf).
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
#' @rdname hazard
#' @export
eval_hazard <- function(object, at) UseMethod("eval_hazard")


#' @rdname hazard
#' @export
get_hazard <- function(object) UseMethod("get_hazard")

#' @export
eval_hazard.dst <- function(object, at) {
	get_hazard(object)(at)
}

#' @export
get_hazard.dst <- function(object) {
	if (variable(object) == "continuous") {
		sf <- get_survival(object)
		pdf <- get_probfn(object)
		function(x) pdf(x) / sf(x)
	}
}
