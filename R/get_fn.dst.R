#' @export
get_cdf.dst <- function(object) object$fun_cumu


#' @export
get_quantile.dst <- function(object) object$fun_quant


#' @export
get_probfn.dst <- function(object) object$fun_prob


#' @export
get_randfn.dst <- function(object) object$fun_rand


#' @export
get_survival.dst <- function(object) object$fun_survival


#' @export
get_chf.dst <- function(object) {
	if (check_continuous(object)) {
		sf <- get_survival(object)
		function(x) -log(sf(x))
	}
}



#' @export
get_hazard.dst <- function(object) {
	if (check_continuous(object)) {
		sf <- get_survival(object)
		pdf <- get_probfn(object)
		function(x) pdf(x) / sf(x)
	}
}
