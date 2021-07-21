# TODO make more robust
make_dst_shift <- function(distribution, constant) {
    with(parameters(distribution), {
        dist <- list(
            components = list(
                distribution = distribution,
                shift = constant
            )
        )
        res <- structure(
            dist,
            variable = variable(distribution),
            class = c(class(d)[1], "negative", class(distribution)[-1])
        )
    })
}

#' @export
mean.shift <- function(object) {
    with(object$components, {
        mean(distribution) + shift
    })
}

#' @export
median.shift <- function(object) {
    with(object$components, {
        median(distribution) + shift
    })
}

#' @export
stdev.shift <- function(object) {
    with(object$components, {
        stdev(distribution)
    })
}

#' @export
range.shift <- function(object) {
    with(object$components, {
        c(lapply(range(distribution), function(x) x + shift))
    })
}

#' @export
variance.shift <- function(object) {
    with(object$components, {
        variance(distribution)
    })
}

#' @export
evi.shift <- function(object) {
    with(object$components, {
        evi(distribution)
    })
}

#' @export
skewness.shift <- function(object) {
    with(object$components, {
        skewness(distribution)
    })
}

#' @export
kurtosis_exc.shift <- function(object) {
    with(object$components, {
        kurtosis_exc(distribution)
    })
}

#' @export
eval_cdf.shift <- function(object, at) {
    with(object$components, {
        eval_cdf(distribution, at - shift)
    })
}

#' @export
eval_quantile.shift <- function(object, at) {
    with(object$components, {
        eval_quantile(distribution, at - shift)
    })
}

#' @export
eval_pmf.shift <- function(object, at) {
    with(object$components, {
        eval_pmf(distribution, at - shift)
    })
}

#' @export
eval_density.shift <- function(object, at) {
    with(object$components, {
        eval_density(distribution, at - shift)
    })
}

#' @export
eval_survival.shift <- function(object, at) {
    with(object$components, {
        eval_survival(distribution, at - shift)
    })
}