#' @export
dst_binom <- function(size, prob) {
    res <- list(parameters = list(size = size, prob = prob))
    new_parametric(l = res, variable = "discrete", class = "binom")
}

#' @export
eval_cdf.binom <- function(object, at, ...) {
    with(parameters(object), {
        pbinom(at, size, prob, lower.tail, log.p, ...)
    })
}

#' @export
eval_density.binom <- function(object, at, ...) {
    with(parameters(object), {
        dbinom(at, size, prob, log, ...)
    })
}

#' @export
eval_quantile.binom <- function(object, at, ...) {
    with(parameters(object), {
        qbinom(at, size, prob, lower.tail, log.p, ...)
    })
}

#' @export
realise.binom <- function(object, at, ...) {
    with(parameters(object), {
        rbinom(at, size, prob, ...)
    })
}