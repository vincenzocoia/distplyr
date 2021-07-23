make_dst_inverse <- function(distribution) {
  with(parameters(distribution), {
    dist <- list(
      distribution = distribution
    )
    new_distribution(dist, variable = variable(distribution), class = "inverse")
  })
}

#' @export
eval_cdf.inverse <- function(object, at) {
  dist <- object$distribution
  eval_cdf(dist, 0) -
    eval_cdf(dist, 1 / at) +
    eval_pmf(dist, 1 / at) +
    as.numeric(at >= 0)
}

#' @export
eval_density.inverse <- function(object, at) {
  eval_density(object$distribution, 1 / at) / at^2
}

#' @export
eval_pmf.inverse <- function(object, at) {
  eval_pmf(object$distribution, 1 / at)
}

#' @export
eval_quantile.inverse <- function(object, at) {
  quantile_0 <- eval_quantile(object$distribution, at = 0)
  1 / eval_quantile(d, quantile_0 + as.numeric(at > quantile_0) - at)
}

#' @export
realise.inverse <- function(object, at, ...) {
  1 / realise(object$distribution, at, ...)
}
