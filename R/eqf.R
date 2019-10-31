#' Empirical quantile function
#'
#' A friend of \code{ecdf}, this function returns a (vectorized) empirical quantile
#' function that's the inverse of ecdf.
#'
#' @param x Numeric vector of the observations for which to make the quantile
#' function
#' @export
eqf <- function(x) as_qdist(
    function(p) {
    	quants <- unname(stats::quantile(x, probs = p, type = 1, na.rm = TRUE))
    	ifelse(p == 0, -Inf, quants)
    }
)
