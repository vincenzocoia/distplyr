#' @export
eval_quantile.graft <- function(x, at, ...) {
	p_cutoff <- x$components$probs[1L]
	res <- numeric(0L)
	for (i in seq_along(at)) {
		if (at[i] <= p_cutoff) {
			new_at <- at[i] / p_cutoff
			this_d <- d$components$distributions[[1L]]
			res[i] <- eval_quantile(this_d, at = new_at)
		} else {
			new_at <- (at[i] - p_cutoff) / (1 - p_cutoff)
			this_d <- d$components$distributions[[2L]]
			res[i] <- eval_quantile(this_d, at = new_at)
		}
	}
	res
}
