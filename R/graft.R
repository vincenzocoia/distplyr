#' Replace the tail of a distribution
#'
#' \code{graft_right()} keeps the left cdf unchanged to the left of
#' sep_y, and makes a continuous connection with the right cdf
#' (rescaled as appropriate).
#' \code{graft_left()} keeps the right cdf unchanged to the right of
#' sep_y, and makes a continuous connection with the left cdf
#' (rescaled as appropriate),
#' @param left_dst,right_dst Distributions to connect
#' @param sep_y Value on the domain of the cdf to connect at.
#' @return Object of class "dst"
#' @export
graft_right <- function(left_dst, right_dst, sep_y) {
	tau1 <- eval_cdf(left_dst, sep_y)
	tau2 <- eval_cdf(right_dst, sep_y)
	cdf <- function(y) {
		lower <- sapply(y <= sep_y, isTRUE) # Make NA's FALSE
		upper <- sapply(y > sep_y, isTRUE)
		y_lower <- y[lower]
		y_upper <- y[upper]
		res <- rep(NA_real_, length(y))
		res[lower] <- eval_cdf(left_dst,  y_lower)
		res[upper] <- (eval_cdf(right_dst, y_upper) - tau2) /
			(1 - tau2) * (1 - tau1) + tau1
		res
	}
	qf <- function(p) {
		lower <- sapply(p <= tau1, isTRUE)
		upper <- sapply(p > tau1, isTRUE)
		p_lower <- p[lower]
		p_upper <- p[upper]
		res <- rep(NA_real_, length(p))
		res[lower] <- eval_quantfn(left_dst,  p_lower)
		res[upper] <- eval_quantfn(right_dst, (p_upper - tau1) /
									   	(1 - tau1) * (1 - tau2) + tau2)
		res
	}
	pf <- NULL
	.has_pdf <- FALSE
	.has_pmf <- FALSE
	if (isTRUE(has_pdf(left_dst) & has_pdf(right_dst))) {
		pf <- function(y) {
			lower <- sapply(y <= sep_y, isTRUE)
			upper <- sapply(y > sep_y, isTRUE)
			y_lower <- y[lower]
			y_upper <- y[upper]
			res <- rep(NA_real_, length(y))
			res[lower] <- eval_probfn(left_dst,  y_lower)
			res[upper] <- eval_probfn(right_dst, y_upper) /
				(1 - tau2) * (1 - tau1)
			res
		}
		.has_pdf <- TRUE
	}
	if (isTRUE(has_pmf(left_dst) & has_pmf(left_dst))) {
		pf <- function(y) {
			lower <- sapply(y <= sep_y, isTRUE)
			upper <- sapply(y > sep_y, isTRUE)
			y_lower <- y[lower]
			y_upper <- y[upper]
			res <- rep(NA_real_, length(y))
			res[lower] <- eval_probfn(left_dst,  y_lower)
			res[upper] <- eval_probfn(right_dst, y_upper) /
				(1 - tau2) * (1 - tau1)
			res
		}
		.has_pmf <- TRUE
	}
	rand <- function(n) qf(stats::runif(n))
	dst(
		fun_cumu  = cdf,
		fun_quant = qf,
		fun_prob  = pf,
		fun_rand  = rand,
		prop = list(evi = evi(right_dst))
	)
}


