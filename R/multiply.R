#' This helper function is not anticipating a negative constant.
multiply <- function(distribution, constant) {
  with(parameters(distribution), {
    if (constant < 0) {
      return(flip(multiply(distribution, -constant)))
    } else if (constant == 0) {
      dst_degenerate(0)
    } else if (constant == 1) {
      distribution
    } else {
      dist <- list(
        components = list(
          distribution = distribution,
          scale = constant
        )
      )
      new_distribution(dist, variable = variable(e1), class = "scale")
    }
  })
}
