#' Distribution Function
#'
#' Get or evaluate a distribution's cdf
#' (Cumulative Distribution Function).
#'
#' @param object Object of class "dst"
#' @param at Vector of values to evaluate the cdf at
#' @return Vector of cdf values
#' @seealso
#' \code{\link{eval_quantile}},
#' \code{\link{eval_probfn}},
#' \code{\link{realise}},
#' \code{\link{eval_hazard}},
#' \code{\link{eval_survival}}
#' @examples
#' d <- dst_unif(0, 4)
#' eval_cdf(d, at = 0:4)
#' cdf <- get_cdf(d)
#' cdf(0:4)
#' @rdname cdf
#' @export
eval_cdf <- function(object, at) UseMethod("eval_cdf")


#' @rdname cdf
#' @export
get_cdf <- function(object) UseMethod("get_cdf")


#' @export
eval_cdf.dst <- function(object, at) {
	cdf <- object[["representations"]][["fun_cumu"]]
	if (!is.null(cdf)) return(cdf(at))
	stop("Can't find a cdf for this distribution.")
}

#' @export
get_cdf.dst <- function(object) {
	cdf <- object[["representations"]][["fun_cumu"]]
	if (!is.null(cdf)) return(cdf)
	function(at) eval_cdf(object, at = at)
}

