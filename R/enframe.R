#' @export
enframe_general <- function(..., eval_fn, at, arg_name, fn_prefix,
							names_repair, fn_args = list()) {
	ellipsis <- rlang::quos(...)
	distributions <- lapply(ellipsis, rlang::eval_tidy)
	n <- length(distributions)
	f <- list()
	for (i in seq_len(n)) {
		f[[i]] <- rlang::exec(
			eval_fn, object = distributions[[i]], at = at, !!!fn_args
		)
	}
	ellipsis_char <- vctrs::vec_cast(ellipsis, to = character())
	names_method <- match(names_repair)
	ellipsis_as_names <- vctrs::vec_as_names(
		ellipsis_char, repair = names_method, quiet = TRUE
	)
	names_with_fn <- paste0(fn_prefix, "_", ellipsis_as_names)
	names(f) <- names_with_fn
	arg <- list(at)
	names(arg) <- arg_name
	res <- as.data.frame(c(arg, f))
	convert_dataframe_to_tibble(res)
}

#' @rdname cdf
#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
						names_repair = c("universal", "unique", "minimal")) {
	enframe_general(..., eval_fn = eval_cdf, at = at,
					arg_name = arg_name, fn_prefix = fn_prefix,
					names_repair = names_repair)
}

#' @rdname pmf
#' @export
enframe_pmf <- function(..., at, arg_name = ".arg", fn_prefix = "pmf",
						names_repair = c("universal", "unique", "minimal"),
						strict = TRUE) {
	enframe_general(..., eval_fn = eval_pmf, at = at,
					arg_name = arg_name,
					fn_prefix = fn_prefix,
					names_repair = names_repair,
					fn_args = list(strict = strict))
}
