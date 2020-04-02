#' Discontinuities in a Distribution
#'
#' Step points are the coordinates marking the
#' positions of step discontinuities in a
#' distribution. They are the upper points at
#' each discontinuity in the cdf, together
#' with the size of the discontinuity gap.
#'
#' @param object A distribution object.
#' @return Data frame with the following columns:
#' \enumerate{
#'   \item \code{y}: Increasing vector of unique values of \code{y}
#'   having positive weight.
#'   \item \code{prob}: Weights corresponding to each outcome.
#'   \item \code{tau}: Cumulative weights; that is, \code{cumsum(prob)}.
#' }
#' @rdname steps
#' @export
steps <- function(object) UseMethod("steps")

#' @export
steps.dst <- function(object) {
	object[["steps"]]
}


#' @param y Vector of outcomes.
#' @param weights Vector of weights, one for each of \code{y}.
#' These need not sum to one, but must not be negative.
#' @details
#' For a vector of outcomes \code{y} with a
#' matching vector of \code{weights},
#' \code{make_steps()} provides a single non-zero, non-NA
#' weight per unique value of \code{y}. Weights sum to 1.
#' @rdname steps
#' @export
make_steps <- function(y, weights) {
	stopifnot(identical(length(y), length(weights)))
	if (identical(length(y), 0L)) {
		return(make_empty_steps_df())
	}
	yw <- data.frame(y = y, w = weights)
	yw <- stats::na.omit(yw)
	yw <- yw[yw[["w"]] != 0, ]
	yw <- yw[order(yw[["y"]]), ]
	y <- yw[["y"]]
	w <- yw[["w"]]
	w <- w / sum(w)
	tau <- cumsum(w)
	rm_id <- which(duplicated(y)) - 1
	if (length(rm_id) > 0) {
		tau <- tau[-rm_id]
	}
	prob <- diff(c(0, tau))
	stopifnot(sum(prob) == 1)
	y <- unique(y)
	stopifnot(length(y) == length(tau))
	data.frame(y = y, prob = prob, tau = tau)
}

#' Make a Data Frame of Steps
#'
#' Places the components of step discontinuities
#' into a data frame. Internal function whose sole
#' purpose is to ensure the consistent naming of
#' step data frame columns.
#' @param y,prob,tau Vectors of equal length from which
#' to construct a data frame.
#' @export
make_steps_df <- function(y, prob, tau) {
	data.frame(y = y, prob = prob, tau = tau)
}

#' Rowless Step Discontinuity Data Frame
#'
#' Internal function for making a step
#' discontinuity data frame having no
#' rows, useful for distributions that
#' do not have any step discontinuities
#' (i.e., continuous distributions)
make_empty_steps_df <- function() {
	make_steps_df(numeric(0L), numeric(0L), numeric(0L))
}
