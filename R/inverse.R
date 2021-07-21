make_dst_inverse <- function(e1) {
    with(parameters(e1), {
        dist <- list(
            components = list(
                distribution = e1,
                shift = e2
            )
        )
        res <- structure(
            dist,
            variable = variable(e1),
            class = c(class(d)[1], "negative", class(distribution)[-1])
        )
    })
}

#' @export
eval_cdf.inverse <- function(object, at) {
    eval_cdf(object, 0) -
        eval_cdf(object, 1 / at) +
        eval_pmf(object, 1 / at) +
        as.numeric(at >= 0)
}

#' @export
eval_density.inverse <- function(object, at) {
    eval_density(object, 1 / at) / at^2
}

#' @export
eval_pmf.inverse <- function(object, at) {
    eval_pmf(object, 1 / at)
}

#' @export
eval_quantile.inverse <- function(object, at) {
    quantile_0 <- eval_quantile(object, at = 0)
    1 / eval_quantile(d, quantile_0 + as.numeric(at > quantile_0) - at)
}