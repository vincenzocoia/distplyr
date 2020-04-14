#' Plot a Distribution
#'
#' Plot a functional representation of a distribution.
#' Wrapper around the \code{graphics::curve} function.
#'
#' @param x Distribution object
#' @param what Name of the representation to plot.
#' @param ... Other arguments to pass to the \code{graphics::curve} function.
#' @export
plot.dst <- function(x,
					 what = c("probfn", "cdf", "survival", "quantile", "hazard", "chf"),
					 ...) {
	ellipses <- list(...)
	fname <- match.arg(what)
	if (is.null(ellipses[["ylab"]])) {
		ellipses[["ylab"]] <- fname
	}
	if (identical(fname, "quantile")) {
		if (is.null(ellipses[["from"]])) {
			ellipses[["from"]] <- 0
		}
		if (is.null(ellipses[["to"]])) {
			ellipses[["to"]] <- 1
		}
		if (is.null(ellipses[["xlab"]])) {
			ellipses[["xlab"]] <- "Probability"
		}
		f <- get_quantile(x)
		ellipses[["expr"]] <- as.name("f")
		do.call(graphics::curve, args = ellipses)
	}
	if (is.null(ellipses[["from"]])) {
		q0 <- eval_quantile(x, at = 0)
		if (identical(q0, -Inf)) {
			ellipses[["from"]] <- eval_quantile(x, at = 0.001)
		} else {
			ellipses[["from"]] <- q0
		}
	}
	if (is.null(ellipses[["to"]])) {
		q1 <- eval_quantile(x, at = 1)
		if (identical(q1, Inf)) {
			ellipses[["to"]] <- eval_quantile(x, at = 0.999)
		} else {
			ellipses[["to"]] <- q1
		}
	}
	if (is.null(ellipses[["xlab"]])) {
		ellipses[["xlab"]] <- "y"
	}
	get_fun <- get(paste0("get_", fname))
	f <- get_fun(x)
	ellipses[["expr"]] <- as.name("f")
	do.call("curve", args = ellipses)
}
