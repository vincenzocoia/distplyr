#' #' @export
#' eval_quantile.graft <- function(x, at, ...) {
#' 	p_cutoff <- x$components$probs[1L]
#' 	at_low <- at[at <= p_cutoff
#' 	at_high
#' 	q <- at
#'
#' 	with(x[["components"]], {
#' 		if (at < left_at) {
#' 			eval_cdf_at_left(object, at)
#' 		} else if (at > right_at) {
#' 			eval_quantile_at_right(object, at)
#' 		} else if (at > left_at &
#' 				   at < right_at) {
#' 			left_quantile <- eval_quantile_at_left(dst_left, left_at)
#' 			base_quantile <- eval_quantile(base, at) -
#' 				eval_quantile(base, left_at)
#' 			left_quantile + base_quantile
#' 		}
#' 	})
#' }
