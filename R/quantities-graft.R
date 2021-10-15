#' @export
evi.graft <- function(distribution, ...) {
	with(distribution$components, {
		nd <- length(distributions)
		evi(distributions[[nd]])
	})
}
