#' @export
print.dst <- function(x, ...) {
	cat(name(x), "Distribution\n")
	px <- parameters(x)
	if (!is.null(px)) {
		cat("\nParameters:\n")
		df <- data.frame(parameter = names(px),
						 value = c(px, recursive = TRUE))
		row.names(df) <- NULL
		print(df)
	}
	cat("\nNumber of Discontinuities: ", nrow(discontinuities(x)))
}
