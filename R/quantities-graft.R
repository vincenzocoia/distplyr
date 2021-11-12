#' @export
evi.graft <- function(distribution) {
	with(distribution$components, {
		distionary::evi(distributions[[2L]])
	})
}
