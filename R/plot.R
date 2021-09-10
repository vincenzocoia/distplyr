#' Plot a Distribution
#'
#' Plot a functional representation of a distribution.
#' Wrapper around the \code{graphics::curve} function.
#'
#' @param x Distribution object
#' @param what Name of the representation to plot.
#' @param ... Other arguments to pass to the \code{graphics::curve} function.
#' plot(dst_norm(5, 5))
#' plot(dst_empirical(-5:2), "cdf", n = 1001)
#' @export
plot.dst <- function(x,
                     what = c("density", "cdf", "survival", "quantile", "hazard", "chf"),
                     ...) {
  ellipsis <- rlang::list2(...)
  fname <- match.arg(what)
  if (identical(fname, "density") && variable(x) != "continuous") {
    warning("Density function does not exist. Plotting cdf instead.")
    fname <- "cdf"
  }
  if (identical(fname, "pmf") && variable(x) != "discrete") {
    warning("Probability mass function does not exist. Plotting cdf instead.")
    fname <- "cdf"
  }
  if (is.null(ellipsis[["ylab"]])) {
    ellipsis[["ylab"]] <- fname
  }
  if (identical(fname, "quantile")) {
    if (is.null(ellipsis[["from"]])) {
      ellipsis[["from"]] <- 0
    }
    if (is.null(ellipsis[["to"]])) {
      ellipsis[["to"]] <- 1
    }
    if (is.null(ellipsis[["xlab"]])) {
      ellipsis[["xlab"]] <- "Probability"
    }
    f <- get_quantile(x)
    ellipsis[["expr"]] <- as.name("f")
    do.call(graphics::curve, args = ellipsis)
  }
  if (is.null(ellipsis[["from"]])) {
    q0 <- eval_quantile(x, at = 0)
    if (identical(q0, -Inf)) {
      ellipsis[["from"]] <- eval_quantile(x, at = 0.001)
    } else {
      ellipsis[["from"]] <- q0
    }
  }
  if (is.null(ellipsis[["to"]])) {
    q1 <- eval_quantile(x, at = 1)
    if (identical(q1, Inf)) {
      ellipsis[["to"]] <- eval_quantile(x, at = 0.999)
    } else {
      ellipsis[["to"]] <- q1
    }
  }
  if (is.null(ellipsis[["xlab"]])) {
    ellipsis[["xlab"]] <- "y"
  }
  get_fun <- get(paste0("get_", fname))
  f <- get_fun(x)
  ellipsis[["expr"]] <- as.name("f")
  do.call("curve", args = ellipsis)
}
