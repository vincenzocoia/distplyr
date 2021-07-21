make_dst_negative <- function(distribution) {
    with(parameters(distribution), {
        dist <- list(
            distribution = distribution
        )
        res <- structure(
            dist,
            variable = variable(distribution),
            class = c(class(d)[1], "negative", class(distribution)[-1])
        )
    })
}

#' @export
eval_quantile.negative <- function(object, at, ...) {
    with(object$distribution, {
        -eval_quantile(object, at = 1 - at)
    })
}

#' @export
eval_survival.negative <- function(object, at, ...) {
    with(object$distribution, {
        eval_cdf(distribution, at = -at) -
            eval_pmf(distribution, at = -at, strict = FALSE)
    })
}

#' @export
eval_pmf.negative <- function(object, at, ...) {
    eval_pmf(object$distribution, at = -at, ...)
}

#' @export
eval_density.negative <- function(object, at, ...) {
    eval_density(object$distribution, at = -at, ...)
}

#' @export
eval_cdf.negative <- function(object, at, ...) {
    eval_pmf(object$distribution, at = -at, strict = FALSE) +
        eval_survival(object$distribution, at = -at)
}