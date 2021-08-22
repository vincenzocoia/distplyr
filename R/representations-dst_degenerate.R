#' @export
realise.degenerate <- function(object, n = 1, ...) {
	rep(parameters(object)$location, n)
}
