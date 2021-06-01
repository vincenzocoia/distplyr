#' @importFrom stats median
#'
#' @rdname moments
#' @export
median.dst <- function(x, ...) {
  eval_quantile(x, at = 0.5)
}
