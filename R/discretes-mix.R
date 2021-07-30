#' @export
has_infinite_discretes <- function(object, from = -Inf, to = Inf) {
	with(object$components, {
		any(vapply(distributions, has_infinite_discretes, FUN.VALUE = logical(1L),
				   from = from, to = to))
	})
}

#' @export
num_discretes.mix <- function(object, from, to, include_from, include_to) {
	if (has_infinite_discretes(object, from = from, to = to)) {
		return(Inf)
	}
	with(object$components, {
		n <- vapply(distributions, num_discretes, FUN.VALUE = numeric(1L),
					from = from, to = to,
					include_from = include_from,
					include_to = include_to)
		discretes <- list()
		for (i in seq_along(distributions)) {
			discretes[[i]] <- next_discrete(
				distributions[[i]], from = from, n = n[[i]], include_from = include_from
			)
		}
		discretes <- unique(c(discretes, recursive = TRUE))
		length(discretes)
	})
}

#' @export
next_discrete.mix <- function(object, from, n, include_from) {
	if (n == 0) return(numeric(0L))
	if (is.infinite(n) &&
		has_infinite_discretes(object, from = from, to = Inf)) {
		stop("Your selection includes an infinite number of discrete points.")
	}
	distributions <- object$components$distributions
	next_discretes <- lapply(distributions, next_discrete, from = from, n = 1L,
							 include_from = include_from)
	next_discretes <- c(next_discretes, recursive = TRUE)
	if (length(next_discretes) == 0) return(numeric(0L))
	is_nan <- is.nan(next_discretes)
	next_discretes <- next_discretes[!is_nan]
	if (length(next_discretes) == 0) return(NaN)
	candidate <- min(next_discretes)
	if (any(is_nan) &&
		has_infinite_discretes(object, from = from, to = candidate)) {
		return(NaN)
	}
	discretes <- candidate
	i <- 2L
	while (i <= n) {
		next_discretes <- lapply(distributions, next_discrete,
								 from = discretes[i - 1L], n = 1L,
								 include_from = FALSE)
		next_discretes <- c(next_discretes, recursive = TRUE)
		if (length(next_discretes) == 0) return(discretes)
		is_nan <- is.nan(next_discretes)
		next_discretes <- next_discretes[!is_nan]
		if (length(next_discretes) == 0) {
			discretes[i] <- NaN
			return(discretes)
		}
		candidate <- min(next_discretes)
		if (any(is_nan) &&
			has_infinite_discretes(object, from = discretes[i - 1L],
								   to = candidate)) {
			discretes[i] <- NaN
			return(discretes)
		}
		discretes[i] <- candidate
		i <- i + 1L
	}
	discretes
}

#' @export
prev_discrete.mix <- function(object, from, n, include_from) {
	if (n == 0) return(numeric(0L))
	if (is.infinite(n) &&
		has_infinite_discretes(object, from = -Inf, to = from)) {
		stop("Your selection includes an infinite number of discrete points.")
	}
	distributions <- object$components$distributions
	prev_discretes <- lapply(distributions, prev_discrete, from = from, n = 1L,
							 include_from = include_from)
	prev_discretes <- c(prev_discretes, recursive = TRUE)
	if (length(prev_discretes) == 0) return(numeric(0L))
	is_nan <- is.nan(prev_discretes)
	prev_discretes <- prev_discretes[!is_nan]
	if (length(prev_discretes) == 0) return(NaN)
	candidate <- min(prev_discretes)
	if (any(is_nan) &&
		has_infinite_discretes(object, from = discretes[i - 1L],
							   to = candidate)) {
		return(NaN)
	}
	discretes <- candidate
	i <- 2L
	while (i <= n) {
		prev_discretes <- lapply(distributions, prev_discrete,
								 from = discretes[i - 1L], n = 1L,
								 include_from = FALSE)
		prev_discretes <- c(prev_discretes, recursive = TRUE)
		if (length(prev_discretes) == 0) return(discretes)
		is_nan <- is.nan(prev_discretes)
		prev_discretes <- prev_discretes[!is_nan]
		if (length(prev_discretes) == 0) {
			discretes[i] <- NaN
			return(discretes)
		}
		candidate <- min(prev_discretes)
		if (any(is_nan) &&
			has_infinite_discretes(object, from = from, to = candidate)) {
			discretes[i] <- NaN
			return(discretes)
		}
		discretes[i] <- candidate
		i <- i + 1L
	}
	discretes
}
