#' @export
print.dst <- function(x, ...) {
	name <- x$name
	param <- x$param
	if (is.null(name)) {
		cat("Unnamed distribution.")
	} else {
		cat(paste("A", name, "distribution."))
	}
	cat("\n\nParameters: ")
	# if (is.null(param)) {
	# 	cat("not available")
	# } else {
	# 	cat("\n")
	# 	df <- data.frame(parameter = names(param),
	# 					 value = unname(param))
	# 	print(df, row.names = FALSE)
	# }
}
