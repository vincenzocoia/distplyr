#' Extract Heights of a Step Function
#'
#' Extracts the heights/"y values"/plateaus of a
#' step function. Sister function to \code{stats::knots()},
#' which returns the breakpoints/"x values".
#'
#' @param object Object of class "stepfun".
#' @return Vector of y values from left to right.
#' @export
plateaus <- function(object) UseMethod("plateaus")

#' @export
plateaus.stepfun <- function(object) {
	e <- environment(object)
	fval <- with(e, f)
	y <- with(e, y)
	if (fval == 1) {
		yright <- with(e, yright)
		return(c(y, yright))
	} else {
		yleft <- with(e, yleft)
		return(c(yleft, y))
	}
}

# stepdist_knots
