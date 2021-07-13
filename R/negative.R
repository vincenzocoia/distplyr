make_dst_negative <- function(e1) {
    with(parameters(e1), {
        dist <- list(
            components = list(
                distribution = e1
            )
        )
        res <- structure(
            dist,
            variable = variable(e1),
            class = c("negative", class(e1))
        )
    })
}

#' @export
eval_cdf.negative <- function(object, at, ...) {
    with(object, {
        eval_pmf(distribution, at = -at, strict = FALSE) +
            eval_survival(distribution, at = -at)
    })
}