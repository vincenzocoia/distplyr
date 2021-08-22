#' @export
#' @inheritParams next_discrete
next_discrete.inverse <- function(object, from, n, include_from) {
	d_nested <- object$distribution
	if (from >= 0) {
		n_available <- num_discretes(d_nested,
									 from = 0, to = 1 / from,
									 include_from = FALSE,
									 include_to = include_from
		)
		n <- min(n, n_available)
		x <- prev_discrete(d_nested,
						   from = 1 / from, n = n,
						   include_from = include_from
		)
	} else {
		x <- prev_discrete(d_nested,
						   from = 1 / from, n = n,
						   include_from = include_from
		)
		n_x <- length(x)
		n_remaining <- n - n_x
		if (n_remaining) {
			n_pos <- num_discretes(d_nested,
								   from = 0, to = Inf,
								   include_from = FALSE,
								   include_to = FALSE
			)
			n_remaining <- min(n_remaining, n_pos)
			x_pos <- prev_discretes(d_nested,
									from = Inf, n = n_remaining,
									include_from = FALSE
			)
			x <- c(x, x_pos)
		}
	}
	1 / x
}

#' @export
#' @inheritParams next_discrete
prev_discrete.inverse <- function(object, from, n, include_from) {
	d_nested <- object$distribution
	if (from <= 0) {
		n_available <- num_discretes(d_nested,
									 from = -1 / abs(from), to = 0,
									 include_from = include_from,
									 include_to = FALSE
		)
		n <- min(n, n_available)
		x <- next_discrete(d_nested,
						   from = -1 / abs(from), n = n,
						   include_from = include_from
		)
	} else {
		x <- next_discrete(d_nested,
						   from = 1 / from, n = n,
						   include_from = include_from
		)
		n_x <- length(x)
		n_remaining <- n - n_x
		if (n_remaining) {
			n_neg <- num_discretes(d_nested,
								   from = -Inf, to = 0,
								   include_from = FALSE,
								   include_to = FALSE
			)
			n_remaining <- min(n_remaining, n_neg)
			x_neg <- next_discretes(d_nested,
									from = -Inf, n = n_remaining,
									include_from = FALSE
			)
			x <- c(x, x_neg)
		}
	}
	1 / x
}

#' @export
num_discretes.inverse <- function(object, from, to, include_from, include_to) {
	d_nested <- object$distribution
	if (to < from) {
		a <- to
		b <- from
		include_a <- include_to
		include_b <- include_from
	} else {
		a <- from
		b <- to
		include_a <- include_from
		include_b <- include_to
	}
	if (b <= 0) {
		n <- num_discretes(d_nested,
						   from = -1 / abs(b), to = -1 / abs(a),
						   include_from = include_b,
						   include_to = include_a
		)
	} else if (a >= 0) {
		n <- num_discretes(d_nested,
						   from = 1 / b, to = 1 / a,
						   include_from = include_b,
						   include_to = include_a
		)
	} else {
		n_neg <- num_discretes(d_nested,
							   from = -Inf, to = 1 / a,
							   include_from = FALSE,
							   include_to = include_a
		)
		n_pos <- num_discretes(d_nested,
							   from = 1 / b, to = Inf,
							   include_from = include_b,
							   include_to = FALSE
		)
		n <- n_neg + n_pos
	}
	n
}
