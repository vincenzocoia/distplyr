#' Check Continuity of Step Function
#'
#' Check for left and right continuity of
#' a step function.
#'
#' @param object Object of class "stepfun" to check.
#' @return Single logical indicating the result.
#' @rdname check_continuous
#' @export
check_left_continuous <- function(object) {
	if (!is.stepfun(object)) {
		stop("Object being tested is not a step function.")
	}
	f <- with(environment(object), f)
	if (f == 1) TRUE else FALSE
}

#' @rdname check_continuous
#' @export
check_right_continuous <- function(object) {
	if (!is.stepfun(object)) {
		stop("Object being tested is not a step function.")
	}
	f <- with(environment(object), f)
	if (f == 0) TRUE else FALSE
}
