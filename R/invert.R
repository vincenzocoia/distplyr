#' @export
invert <- function(distribution) {
  with(parameters(distribution), {
    dist <- list(
      distribution = distribution
    )
    new_distribution(dist, variable = variable(distribution), class = "inverse")
  })
}
