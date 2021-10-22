#' @export
#' @inheritParams next_discrete
next_discrete.shift <- function(distribution, from, n = 1L,
								include_from = FALSE, ...) {
	with(distribution$components, {
		next_discrete(distribution,
					  from = from,
					  n = n,
					  include_from = include_from
		) + shift
	})
}

#' @export
#' @inheritParams next_discrete
prev_discrete.shift <- function(distribution, from, n = 1L,
								include_from = FALSE, ...) {
	with(distribution$components, {
		prev_discrete(distribution,
					  from = from,
					  n = n,
					  include_from = include_from
		) + shift
	})
}
