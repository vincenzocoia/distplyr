#' Discontinuities in a Distribution
#'
#' Discontinuity points are the coordinates marking the
#' positions of step discontinuities in a
#' distribution. They are the upper points at
#' each discontinuity in the cdf, together
#' with the size of the discontinuity gap.
#'
#' @param object A distribution object.
#' @return Data frame with the following columns:
#' \enumerate{
#'   \item \code{location}: Increasing vector of unique values of \code{y}
#'   having positive weight.
#'   \item \code{size}: Weights corresponding to each outcome.
#' }
#' @rdname discontinuities
#' @examples
#' a <- stepdst(1:5)
#' discontinuities(a)
#' b <- dst_norm(0, 1)
#' discontinuities(b)
#' c <- graft_right(a, b, sep_y = 3.5)
#' discontinuities(c)
#' @export
discontinuities <- function(object) UseMethod("discontinuities")

#' @export
discontinuities.dst <- function(object) {
	object[["discontinuities"]]
}


#' @param y Vector of outcomes.
#' @param weights Vector of weights, one for each of \code{y}.
#' These need not sum to one, but must not be negative.
#' @param sum_to_one Logical; should the weights be normalized
#' to sum to 1? Default is FALSE.
#' @details
#' For a vector of outcomes \code{y} with a
#' matching vector of \code{weights},
#' \code{aggregate_weights()} provides a single non-zero, non-NA
#' weight per unique value of \code{y}.
#' @rdname discontinuities
#' @export
aggregate_weights <- function(y, weights, sum_to_one = FALSE) {
	stopifnot(identical(length(y), length(weights)))
	if (identical(length(y), 0L)) {
		return(make_empty_discontinuities_df())
	}
	yw <- data.frame(y = y, w = weights)
	yw <- stats::na.omit(yw)
	yw <- yw[yw[["w"]] != 0, ]
	y <- yw[["y"]]
	w <- yw[["w"]]
	if (sum_to_one) w <- w / sum(w)
	df <- stats::aggregate(data.frame(size = w), by = list(location = y), FUN = sum)
	df <- df[order(df[["location"]]), ]
	stopifnot(is_discontinuities_df(df))
	df
}

#' Make a Data Frame of Discontinuities
#'
#' Places the components of step discontinuities
#' into a data frame, and checks that the result is named
#' appropriately.
#' @param location,size Vectors of equal length from which
#' to construct a data frame.
#' @export
make_discontinuities_df <- function(location, size) {
	df <- data.frame(location = location, size = size)
	stopifnot(is_discontinuities_df(df))
	df
}

#' Check if a Data Frame is a Discontinuity Data Frame
#'
#' The official names in a discontinuity data frame are
#' determined (and checked) here.
#' @param df Data frame to check
#' @return Logical.
#' @export
is_discontinuities_df <- function(df) {
	if (!is.data.frame(df)) return(FALSE)
	if (!identical(names(df), c("location", "size"))) return(FALSE)
	if (identical(nrow(df), 0L)) return(TRUE)
	with(df, {
		if (sum(size) > 1) return(FALSE)
		if (any(size <= 0)) return(FALSE)
		if (!identical(length(location),
					   length(unique(location)))) return(FALSE)
	})
	TRUE
}

#' Rowless Step Discontinuity Data Frame
#'
#' Internal function for making a step
#' discontinuity data frame having no
#' rows, useful for distributions that
#' do not have any step discontinuities
#' (i.e., continuous distributions)
make_empty_discontinuities_df <- function() {
	make_discontinuities_df(numeric(0L), numeric(0L))
}

#' Determine Variable Type from Discontinuities Data Frame
#'
#' Internal function that uses a data frame of
#' discontinuities to determine whether the
#' underlying random variable is continuous,
#' discrete, or mixed.
#' @param df A data frame of discontinuities, as in
#' the output of \code{\link{discontinuities}}.
#' @return One of \code{"continuous"},
#' \code{"discrete"}, or \code{"mixed"}.
discontinuities_to_variable <- function(df) {
	n <- nrow(df)
	if (identical(n, 0L)) return("continuous")
	probs <- df[["size"]]
	if (sum(probs) == 1) {
		"discrete"
	} else {
		"mixed"
	}
}
