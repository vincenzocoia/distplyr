#' @export
evi.graft <- function(object, ...) {
	with(object$components, {
		nd <- length(distributions)
		evi(distributions[[nd]])
	})
}
