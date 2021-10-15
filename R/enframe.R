#' @export
enframe_general <- function(..., at, arg_name, fn_prefix, sep,
							eval_fn, fn_args = list()) {
	ellipsis <- rlang::quos(...)
	ellipsis <- rlang::quos_auto_name(ellipsis)
	distributions <- lapply(ellipsis, rlang::eval_tidy)
	is_distributions <- vapply(distributions, is_distribution,
							   FUN.VALUE = logical(1L))
	if (!all(is_distributions)) {
		stop("`enframe_*()` functions only accept distributions. ",
			 "Entries that are not distributions: ",
			 paste(which(!is_distributions), collapse = ", "))
	}

	n <- length(distributions)
	if (n == 0L) {
		stop("Need at least one distribution in the `enframe_*()` function.")
	}
	f <- list()
	for (i in seq_len(n)) {
		f[[i]] <- rlang::exec(
			eval_fn, object = distributions[[i]], at = at, !!!fn_args
		)
	}
	if (n == 1L) {
		eval_col_names <- fn_prefix
	} else {
		ellipsis_names <- rlang::names2(ellipsis)
		dist_names <- vctrs::vec_as_names(ellipsis_names, repair = "unique")
		eval_col_names <- paste0(fn_prefix, sep, dist_names)
	}
	names(f) <- eval_col_names
	arg <- list(at)
	names(arg) <- arg_name
	res <- as.data.frame(c(arg, f))
	convert_dataframe_to_tibble(res)
}

#' @rdname cdf
#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
						sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_cdf)
}

#' @rdname pmf
#' @export
enframe_pmf <- function(..., at, arg_name = ".arg", fn_prefix = "pmf",
						sep = "_", strict = TRUE) {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_pmf,
					fn_args = list(strict = strict))
}

#' @rdname odds
#' @export
enframe_odds <- function(..., at, arg_name = ".arg", fn_prefix = "odds",
						 sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_odds)
}

#' @rdname survival
#' @export
enframe_survival <- function(..., at, arg_name = ".arg", fn_prefix = "survival",
							 sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_survival)
}

#' @rdname density
#' @export
enframe_density <- function(..., at, arg_name = ".arg", fn_prefix = "density",
							sep = "_", strict = TRUE) {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_density,
					fn_args = list(strict = strict))
}

#' @rdname hazard
#' @export
enframe_hazard <- function(..., at, arg_name = ".arg", fn_prefix = "hazard",
						   sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_hazard)
}

#' @rdname chf
#' @export
enframe_chf <- function(..., at, arg_name = ".arg", fn_prefix = "chf",
						sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_chf)
}

#' @rdname quantile
#' @export
enframe_quantile <- function(..., at, arg_name = ".arg", fn_prefix = "quantile",
							 sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_quantile)
}
