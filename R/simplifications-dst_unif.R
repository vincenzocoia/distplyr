#' @inherit flip
#' @export
flip.unif <- function(distribution) {
	mutate_parameters(distribution, min = -max, max = -min)
}

#' @inherit shift
#' @export
shift.unif <- function(distribution, constant) {
	mutate_parameters(distribution, min = min + constant, max = max + constant)
}

#' @inherit multiply
#' @export
multiply.unif <- function(distribution, constant) {
	if (constant == 0) return(distionary::dst_degenerate(0))
	if (constant < 0) return(flip(multiply(distribution, -constant)))
	mutate_parameters(distribution, min = min * constant, max = max * constant)
}
