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
#' a <- dst_empirical(1:5)
#' discontinuities(a)
#' b <- dst_norm(0, 1)
#' discontinuities(b)
#' c <- graft_right(a, b, sep_y = 3.5)
#' discontinuities(c)
#' @export
discontinuities <- function(object, from, to, ...) UseMethod("discontinuities")

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
  na_y <- is.na(y)
  na_w <- is.na(weights)
  na <- na_y | na_w
  clean_y <- y[!na]
  clean_w <- weights[!na]
  zero_w <- clean_w == 0
  cleaner_y <- clean_y[!zero_w]
  cleaner_w <- clean_w[!zero_w]
  if (length(cleaner_y) == 0L) {
    return(make_empty_discontinuities_df())
  }
  if (sum_to_one) {
    cleaner_w <- cleaner_w / sum(cleaner_w)
  }
  df <- stats::aggregate(
    data.frame(size = cleaner_w),
    by = list(location = cleaner_y),
    FUN = sum
  )
  df <- df[order(df[["location"]]), , drop = FALSE]
  # stopifnot(is_discontinuities_df(df))
  convert_dataframe_to_tibble(df)
}

#' Rowless Step Discontinuity Data Frame
#'
#' Internal function for making a step
#' discontinuity data frame having no
#' rows, useful for distributions that
#' do not have any step discontinuities
#' (i.e., continuous distributions)
make_empty_discontinuities_df <- function() {
  df <- data.frame(location = numeric(), size = numeric())
  # stopifnot(is_discontinuities_df(df))
  df <- convert_dataframe_to_tibble(df)
  df
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
#' @export
discontinuities_to_variable <- function(df) {
  n <- nrow(df)
  if (identical(n, 0L)) {
    return("continuous")
  }
  probs <- df[["size"]]
  if (sum(probs) == 1) {
    "discrete"
  } else {
    "mixed"
  }
}
