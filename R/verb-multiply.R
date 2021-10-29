#' @rdname linear_transform
#' @export
multiply <- function(distribution, constant) {
  if (constant < 0) {
    return(flip(multiply(distribution, -constant)))
  } else if (constant == 0) {
  	distionary::dst_degenerate(0)
  } else if (constant == 1) {
    distribution
  } else if (is.infinite(constant)) {
    stop("Cannot multiply a distribution by infinity.")
  } else {
    dist <- list(
      components = list(
        distribution = distribution,
        scale = constant
      )
    )
    distionary::new_distribution(
    	dist, variable = distionary::variable(distribution), class = "scale"
    )
  }
}
