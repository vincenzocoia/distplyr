#' @export
shift <- function(distribution, constant) UseMethod("shift")

#' @export
shift.dst <- function(distribution, constant) {
  with(parameters(distribution), {
    if (constant == 0) {
      return(dst_degenerate(0))
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