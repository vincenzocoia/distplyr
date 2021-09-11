#' @export
enframe_general <- function(..., eval_fn, at, arg_name, fn_name,
							fn_args = list()) {
	ellipsis <- rlang::quos(...)
	distributions <- lapply(ellipsis, rlang::eval_tidy)
	n <- length(distributions)
	f <- list()
	for (i in seq_len(n)) {
		f[[i]] <- rlang::exec(
			eval_fn, object = distributions[[i]], at = at, !!!fn_args
		) #object = distributions[[i]], !!!fn_args)
	}
	if (n == 1L) {
		names(f) <- fn_name
	} else {
		for (i in 1:n) {
			if (rlang::quo_is_symbol(ellipsis[[i]])) {
				sym <- rlang::as_name(ellipsis[[i]])
				names(f)[i] <- paste0(fn_name, ".", sym)
			} else {
				names(f)[i] <- paste0(fn_name, "..", i)
			}
		}
	}
	arg <- list(at)
	names(arg) <- arg_name
	res <- as.data.frame(c(arg, f))
	convert_dataframe_to_tibble(res)
}

#' @rdname cdf
#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_name = ".cdf") {
	enframe_general(..., eval_fn = eval_cdf, at = at,
					arg_name = arg_name, fn_name = fn_name)
}

#' @rdname pmf
#' @export
enframe_pmf <- function(..., at, arg_name = ".arg", fn_name = ".pmf",
						strict = TRUE) {
	enframe_general(..., eval_fn = eval_pmf, at = at,
					arg_name = arg_name, fn_name = fn_name,
					fn_args = list(strict = strict))
}
