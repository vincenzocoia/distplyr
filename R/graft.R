#' Graft Distributions
#'
#' Replace the tail of a distribution.
#' \code{graft_right()} keeps the left cdf unchanged to the left of
#' sep_y, and makes a continuous connection with the right cdf
#' (rescaled as appropriate).
#' \code{graft_left()} keeps the right cdf unchanged to the right of
#' sep_y, and makes a continuous connection with the left cdf
#' (rescaled as appropriate),
#' @param dst_left,dst_right Distributions to connect
#' @param sep_y Value on the domain of the cdf to connect at.
#' @return A grafted distribution object.
#' @rdname graft
#' @export
graft_right <- function(dst_left, dst_right, sep_y) {
	tau_left <- eval_cdf(dst_left, sep_y)
	tau_right <- eval_cdf(dst_right, sep_y)
	steps_left <- discontinuities(dst_left)
	steps_left <- steps_left[steps_left[["location"]] <= sep_y, ]
	steps_right <- discontinuities(dst_right)
	steps_right <- steps_right[steps_right[["location"]] > sep_y, ]
	steps_right[["size"]] <- steps_right[["size"]] * (1 - tau_left) /
		(1 - tau_right)
	steps_combined <- rbind(steps_left, steps_right)
	stopifnot(is_discontinuities_df(steps_combined))
	v <- steps_to_variable(steps_combined)
	res <- list(discontinuities = steps_combined,
				components = list(dst_left  = dst_left,
								  dst_right = dst_right,
								  tau_left  = tau_left,
								  tau_right = tau_right,
								  sep_y     = sep_y,
								  base      = "left"))
	new_dst(res, variable = v, class = "graft")
}


#' @param object Object to be tested
#' @rdname graft
#' @export
is_graft <- function(object) inherits(object, "graft")

#' @rdname graft
#' @export
is.graft <- function(object) inherits(object, "graft")

#' @export
get_cdf.graft <- function(object) {
	with(object[["components"]], {
		if (identical(base, "left")) {
			function(y) {
				lower <- vapply(y <= sep_y, isTRUE, FUN.VALUE = logical(1))
				upper <- vapply(y > sep_y,  isTRUE, FUN.VALUE = logical(1))
				y_lower <- y[lower]
				y_upper <- y[upper]
				res <- rep(NA_real_, length(y))
				res[lower] <- eval_cdf(dst_left,  y_lower)
				res[upper] <- (eval_cdf(dst_right, y_upper) - tau_right) /
					(1 - tau_right) * (1 - tau_left) + tau_left
				res
			}
		} else {
			stop("Not yet programmed.")
		}
	})
}

#' @export
get_quantile.graft <- function(object) {
	with(
		object[["components"]],
		if (identical(base, "left")) {
			function(p) {
				lower <- vapply(p <= tau_left, isTRUE, FUN.VALUE = logical(1))
				upper <- vapply(p > tau_left,  isTRUE, FUN.VALUE = logical(1))
				p_lower <- p[lower]
				p_upper <- p[upper]
				res <- rep(NA_real_, length(p))
				res[lower] <- eval_quantile(dst_left,  p_lower)
				res[upper] <- eval_quantile(dst_right, (p_upper - tau_left) /
												(1 - tau_left) * (1 - tau_right) + tau_right)
				res
			}
		} else {
			stop("Not yet programmed.")
		}
	)
}

#' @export
get_probfn.graft <- function(object) {
	if (!identical(variable(object), "continuous")) {
		return(NULL)
	}
	with(
		object[["components"]],
		if (identical(base, "left")) {
			function(y) {
				lower <- vapply(y <= sep_y, isTRUE, FUN.VALUE = logical(1))
				upper <- vapply(y > sep_y,  isTRUE, FUN.VALUE = logical(1))
				y_lower <- y[lower]
				y_upper <- y[upper]
				res <- rep(NA_real_, length(y))
				res[lower] <- eval_probfn(dst_left,  y_lower)
				res[upper] <- eval_probfn(dst_right, y_upper) /
					(1 - tau_right) * (1 - tau_left)
				res
			}
		} else {
			stop("Not yet programmed.")
		}
	)
}

#' @export
get_evi.graft <- function(object) {
	with(
		object[["components"]],
		get_evi(dst_right)
	)
}

# Moment-based quantities may require integration - TBD

# Using .dst method for:
# - get_hazard
# - get_chf
# - get_randfn
# - get_survival
# - get_median

