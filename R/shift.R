make_dst_shift <- function(distribution, constant) {
  with(parameters(distribution), {
    if (constant == 0) {
      return(distribution)
    }
    dist <- list(
      components = list(
        distribution = distribution,
        shift = constant
      )
    )
    new_distribution(dist, variable = variable(distribution), class = "shift")
  })
}


