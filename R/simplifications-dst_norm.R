#' @inherit flip
#' @export
flip.norm <- function(distribution) {
	mutate_parameters(distribution, mean = -mean)
}

#' @inherit shift
#' @export
shift.norm <- function(distribution, constant) {
	mutate_parameters(distribution, mean = mean + constant)
}

#' @inherit multiply
#' @export
multiply.norm <- function(distribution, constant) {
	if (constant == 0) return(distionary::dst_degenerate(0))
	mutate_parameters(distribution,
					  mean = mean * constant,
					  variance = variance * constant^2)
}
