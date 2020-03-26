#' @export
eval_cdf.dst <- function(object, at) {
	get_cdf(object)(at)
}

#' @export
eval_survival.dst <- function(object, at) {
	get_survival(object)(at)
}

#' @export
eval_probfn.dst <- function(object, at) {
	get_probfn(object)(at)
}


#' @export
eval_hazard.dst <- function(object, at) {
	get_hazard(object)(at)
}

#' @export
eval_chf.dst <- function(object, at) {
	get_chf(object)(at)
}

#' @export
eval_randfn.dst <- function(object, at) {
	get_randfn(object)(at)
}

#' @export
eval_quantile.dst <- function(object, at) {
	get_quantile(object)(at)
}
