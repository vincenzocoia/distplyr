#' @export
get_cdf.dst <- function(object) object$fun_cumu

#' @param tol tolerance
#' @param maxiter Maximum number of iterations
#' @rdname get_quantile
#' @export
get_quantile.dst <- function(object, tol = 1e-6, maxiter = 1000, ...) {
	f <- object[["representations"]][["fun_quant"]]
	if (!is.null(f)) return(f)
	cdf <- get_cdf(object)
	discon <- discontinuities(object)
	n_breaks <- nrow(discon)
	breaks <- discon[["location"]]
	cdf_high <- cdf(breaks)
	cdf_low <- cdf_high - discon[["size"]]
	function(x) {
		ox <- order(x)
		x <- x[ox]
		dup_x <- duplicated(x)
		n_x <- length(x)
		if (identical(n_x, 0L)) return(numeric(0L))
		res <- x
		i <- 0L
		batch_id <- 0L
		while (i < n) {
			remaining_xs <- x[-seq_len(i)]
			next_x <- remaining_xs[1L]
			remaining_cdf_highs <- cdf_high[-seq_len(batch_id)]
			higher_batch_ids <- which(next_x <= remaining_cdf_highs)
			if (identical(length(higher_batch_ids), 0L)) {
				batch_id <- n_breaks + 1L
				if (identical(i, 0L)) {
					low <- get_lower(object, level = x[1L])
				} else {
					low <- breaks[n_breaks]
				}
				high <- get_higher(object, level = x[n_x])
				n_x_in_batch <- n - i
				n_x_below_discont <- n_x_in_batch
			} else {
				delta_batch_id <- higher_batch_ids[1L]
				batch_id <- batch_id + delta_batch_id
				this_cdf_high <- cdf_high[batch_id]
				this_cdf_low <- cdf_low[batch_id]
				this_break <- breaks[batch_id]
				low <- breaks[batch_id - 1L]
				high <- this_break
				n_x_in_batch <- sum(remaining_xs <= this_cdf_high)
				n_x_below_discont <- sum(remaining_xs <= this_cdf_low)
				n_x_in_discont <- n_x_in_batch - n_x_below_discont
				x_ids_in_discont <- i + n_x_below_discont + seq_len(n_x_in_discont)
				res[x_ids_in_discont] <- this_break
			}
			for (x_id in i + seq_len(n_x_below_discont)) {
				if (dup_x[x_id]) {
					res[x_id] <- res[x_id - 1L]
				} else {
					answer <- left_inverse(cdf, at = x[x_id],
										   low = low, high = high,
										   tol = tol, maxiter = maxiter)
					low <- answer
					res[x_id] <- answer
				}
			}
			i <- i + n_x_in_batch
		}
		unordered_res <- res
		unordered_res[ox] <- res
		unordered_res
	}
}

#' Algorithm to Compute Left Inverse
#'
#' Calculates the smallest value for which a function
#' \code{f} evaluates to be greater than or equal to
#' \code{at} -- that is, the left inverse of \code{f}
#' at \code{at}. Intended for internal use only.
#' @param f A function to compute the left inverse of.
#' @param at Value for which to calculate the left inverse.
#' Not vectorized.
#' @param low,high Single numeric values forming a range
#' within which to search for the solution.
#' @param tol Tolerance for the solution. The actual
#' solution will be within plus or minus half this value.
#' @param maxiter Number of iterations to attempt before
#' quitting, if the tolerance has not been reached. by then.
#' @details This algorithm works by progressively
#' cutting the specified range in half, so that the width
#' of the range after k iterations is 1/2^k times the
#' original width.
#' @export
left_inverse <- function(f, at, low, high, tol, maxiter) {
	stopifnot(low < high)
	if (is.na(at)) return(at)
	w <- high - low
	i <- 0
	while(w > tol && i <= maxiter) {
		i <- i + 1
		mid <- (high + low) / 2
		val <- f(mid)
		if (val >= x) {
			high <- mid
		} else {
			low <- mid
		}
	}
	if (i == maxiter && w > tol) {
		warning("Maximum number of iterations reached before tolerance was achieved.")
	}
	mid
}

#' Find an Encapsulating Range of Quantiles
#'
#' Given a quantile level, these functions
#' will find a quantile that evaluates to
#' be higher/lower than the provided quantile level.
#' Intended for internal use only.
#' @param object Distribution
#' @param level Quantile level defining the
#' @rdname practical_limits
#' @export
get_higher <- function(object, level) {
	# discon <- discontinuities(object)
	# if (!identical(nrow(discon), 0L)) {
	# 	r <- range(discon[["location"]])
	# 	low <- r[1] - 0.0001
	# 	high <- r[2] + 0.0001
	# } else {
	# 	rf <- get_randfn(object)
	# 	if (!is.null(rf)) {
	# 		x1 <- rf(1)
	# 		x2 <- rf(1)
	# 		while(x1 == x2) {
	# 			x2 <- rf(1)
	# 		}
	# 		low <- min(x1, x2)
	# 		high <- max(x1, x2)
	# 	}
	# }
	warning("This function doesn't work properly yet!")
	# if (cdf(low) >= x) {
	# 	stop("cdf at low value must evaluate to <p.")
	# }
	# if (cdf(high) < x) {
	# 	stop("cdf at high value must evaluate to >=p.")
	# }
	1000
}

#' @rdname practical_limits
#' @export
get_lower <- function(object, level) {
	warning("This function doesn't work properly yet!")
	-1000
}


#' @export
get_probfn.dst <- function(object) object$fun_prob


#' @export
get_randfn.dst <- function(object) {
	r <- object[["representations"]][["fun_rand"]]
	if (is.null(r)) {
		qf <- get_quantile(object)
		function(n) qf(stats::runif(n))
	} else {
		r
	}
}


#' @export
get_survival.dst <- function(object) {
	sf <- object[["representations"]][["fun_survival"]]
	if (is.null(sf)) {
		cdf <- get_cdf(object)
		function(x) 1 - cdf(x)
	} else {
		sf
	}
}


#' @export
get_chf.dst <- function(object) {
	if (variable(object) == "continuous") {
		sf <- get_survival(object)
		function(x) -log(sf(x))
	}
}



#' @export
get_hazard.dst <- function(object) {
	if (variable(object) == "continuous") {
		sf <- get_survival(object)
		pdf <- get_probfn(object)
		function(x) pdf(x) / sf(x)
	}
}
