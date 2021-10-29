#' Linear Transformations
#'
#' `shift()` a distribution by adding a constant, or `multiply()` a
#' distribution by a constant. `flip()` is a specific case of multiplying
#' a distribution by `-1`, resulting in "flipping" the distribution about 0.
#'
#' @param distribution A probability distribution.
#' @param constant A single numeric by which to shift or multiply the
#' distribution by.
#' @return A distribution, shifted or multiplied by the constant.
#' Specifically, a distribution with subclass "shift", "scale", or "flip".
#' @note You can also use the binary operations `+`, `-`, `*`, and `/`
#' to access these transformations.
#' @details Specifically, if `X` is a random variable coming from a
#' distribution, then the resulting distributions are as follows:
#'
#' - For `shift()`, is the distribution of `X + constant`.
#' - For `multiply()`, is the distribution of `X * constant`.
#' - For `flip()`, is the distribution of `-X`.
#'
#' Although the `multiply()` function accepts negative constants,
#' the corresponding "scale" distribution class only holds positive
#' constants, delegating a potential negative sign to the "flip" class.
#' @seealso `invert()`
#' @examples
#' # Shift a Poisson distribution by 1.
#' shift(dst_pois(1), 1)
#' dst_pois(1) + 1
#'
#' # Multiply a Uniform distribution by 2.
#' multiply(dst_unif(0, 1), 2)
#' dst_unif(0, 1) * 2
#'
#' # Flip a Normal distribution.
#' flip(dst_norm(4, 1))
#' -dst_norm(4, 1)
#'
#' # Combine multiple operations:
#' 4 - 2 * dst_pois(1.1)
#' @rdname linear_transform
#' @export
shift <- function(distribution, constant) {
  with(distionary::parameters(distribution), {
    if (constant == 0) {
      return(distionary::dst_degenerate(0))
    }
    dist <- list(
      components = list(
        distribution = distribution,
        shift = constant
      )
    )
    distionary::new_distribution(
    	dist, variable = distionary::variable(distribution), class = "shift"
    )
  })
}


