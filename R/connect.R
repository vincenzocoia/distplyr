#' Replace the tail of a distribution
#'
#' \code{right_connect()} keeps the left cdf unchanged to the left of
#' sep_val, and makes a continuous connection with the right cdf
#' (rescaled as appropriate).
#' \code{left_connect()} keeps the right cdf unchanged to the right of
#' sep_val, and makes a continuous connection with the left cdf
#' (rescaled as appropriate),
#' @param left_dst,right_dst Distributions to connect
#' @param sep_x Value on the domain of the cdf to connect at.
#' @return Object of class "dst"
#' @export
right_connect <- function(left_dst, right_dst, sep_x) {
	tau1 <- fun_cumu(left_dst, sep_x)
	tau2 <- fun_cumu(right_dst, sep_x)
	cdf <- function(x) {
		lower <- sapply(x <= sep_x, isTRUE) # Make NA's FALSE
		upper <- sapply(x > sep_x, isTRUE)
		x_lower <- x[lower]
		x_upper <- x[upper]
		res <- rep(NA_real_, length(x))
		res[lower] <- fun_cumu(left_dst,  x_lower)
		res[upper] <- (fun_cumu(right_dst, x_upper) - tau2) /
			(1 - tau2) * (1 - tau1) + tau1
		res
	}
	qf <- function(p) {
		lower <- sapply(p <= tau1, isTRUE)
		upper <- sapply(p > tau1, isTRUE)
		p_lower <- p[lower]
		p_upper <- p[upper]
		res <- rep(NA_real_, length(p))
		res[lower] <- fun_quant(left_dst,  p_lower)
		res[upper] <- fun_quant(right_dst, (p_upper - tau1) /
									   	(1 - tau1) * (1 - tau2) + tau2)
		res
	}
	pf <- NULL
	.has_pdf <- FALSE
	.has_pmf <- FALSE
	if (has_pdf(left_dst) & has_pdf(right_dst)) {
		pf <- function(x) {
			lower <- sapply(x <= sep_x, isTRUE)
			upper <- sapply(x > sep_x, isTRUE)
			x_lower <- x[lower]
			x_upper <- x[upper]
			res <- rep(NA_real_, length(x))
			res[lower] <- fun_prob(left_dst,  x_lower)
			res[upper] <- fun_prob(right_dst, x_upper) /
				(1 - tau2) * (1 - tau1)
			res
		}
		.has_pdf <- TRUE
	}
	if (has_pmf(left_dst) & has_pmf(left_dst)) {
		pf <- function(x) {
			lower <- sapply(x <= sep_x, isTRUE)
			upper <- sapply(x > sep_x, isTRUE)
			x_lower <- x[lower]
			x_upper <- x[upper]
			res <- rep(NA_real_, length(x))
			res[lower] <- fun_prob(left_dst,  x_lower)
			res[upper] <- fun_prob(right_dst, x_upper) /
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


