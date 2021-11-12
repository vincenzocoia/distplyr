#' @rdname linear_transform
#' @export
flip <- function(distribution) {
  with(distionary::parameters(distribution), {
    dist <- list(
      distribution = distribution
    )
    distionary::new_distribution(
    	dist, variable = distionary::variable(distribution), class = "negative"
    )
  })
}
