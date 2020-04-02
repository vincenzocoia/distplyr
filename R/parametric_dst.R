#' Constructor Function for "dst" Objects
#'
#' @param l List containing the components of a distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#' or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_parametric_dst <- function(l,
							   variable = c("continuous", "discrete", "mixed"),
							   class = character()) {
	v <- match.arg(variable)
	new_dst(
		l,
		variable = v,
		class = c(class, "parametric")
	)
}

#' Parameters of a Parametric Distribution
#'
#' @param object Distribution object
#' @export
parameters <- function(object) UseMethod("parameters")

#' @export
parameters.parametric <- function(object) {
	object[["parameters"]]
}
