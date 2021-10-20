#' @export
evi.graft <- function(distribution, ...) {
	with(distribution$components, {
		evi(distributions[[2L]])
	})
}
