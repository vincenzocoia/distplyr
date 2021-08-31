#' @export
realise.degenerate <- function(object, n = 1, ...) {
    rep(object$probabilities$location[[1]], n)
}