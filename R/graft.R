#' Graft Distributions
#'
#' Replace the tail of a distribution.
#' \code{graft_right()} keeps the left cdf unchanged to the left of
#' sep_y, and makes a continuous connection with the right cdf
#' (rescaled as appropriate).
#' \code{graft_left()} keeps the right cdf unchanged to the right of
#' sep_y, and makes a continuous connection with the left cdf
#' (rescaled as appropriate),
#' @param dst_left,dst_right Distributions to connect
#' @param sep_y Value on the domain of the cdf to connect at.
#' @return A grafted distribution object.
#' @examples
#' require(datasets)
#' base <- dst_empirical(mpg, data = mtcars)
#' right <- dst_gpd(25, 5, 1)
#' g <- graft_right(base, right, sep_y = 25)
#' plot(g, "cdf", n = 1001, to = 34)
#' plot(base, "cdf", n = 1001, lty = 2, add = TRUE)
#' @rdname graft
#' @export
graft_right <- function(dst_left, dst_right, sep_y) {
  tau_left <- eval_cdf(dst_left, sep_y)
  tau_right <- eval_cdf(dst_right, sep_y)
  steps_left <- discontinuities(dst_left)
  steps_left <- steps_left[steps_left[["location"]] <= sep_y, ]
  steps_right <- discontinuities(dst_right)
  steps_right <- steps_right[steps_right[["location"]] > sep_y, ]
  steps_right[["size"]] <- steps_right[["size"]] * (1 - tau_left) /
    (1 - tau_right)
  steps_combined <- rbind(steps_left, steps_right)
  stopifnot(is_discontinuities_df(steps_combined))
  v <- discontinuities_to_variable(steps_combined)
  res <- list(
    name = "Graft",
    discontinuities = steps_combined,
    components = list(
      dst_left = dst_left,
      dst_right = dst_right,
      tau_left = tau_left,
      tau_right = tau_right,
      sep_y = sep_y,
      base = "left"
    )
  )
  if (identical(v, "discrete")) {
    new_finite(res, variable = v, class = "graft")
  } else {
    new_distribution(res, variable = v, class = "graft")
  }
}

#' @export
print.graft <- function(x, ...) {
  cat("Graft Distribution\n")
  cat("\nBase: ")
  cat(name(x[["components"]][["dst_left"]]), "Distribution")
  cat("\n(Right) Tail: ")
  cat(name(x[["components"]][["dst_right"]]), "Distribution")
  cat("\nSeparated at outcome: ")
  cat(x[["components"]][["sep_y"]])
  cat("\n")
  cat("\nNumber of Discontinuities: ", nrow(discontinuities(x)))
}


#' @param object Object to be tested
#' @rdname graft
#' @export
is_graft <- function(object) inherits(object, "graft")

#' @rdname graft
#' @export
is.graft <- function(object) inherits(object, "graft")

#' @export
eval_cdf.graft <- function(object, at) {
  with(object[["components"]], {
    if (identical(base, "left")) {
      lower <- vapply(at <= sep_y, isTRUE, FUN.VALUE = logical(1))
      upper <- vapply(at > sep_y, isTRUE, FUN.VALUE = logical(1))
      y_lower <- at[lower]
      y_upper <- at[upper]
      res <- rep(NA_real_, length(at))
      res[lower] <- eval_cdf(dst_left, y_lower)
      res[upper] <- (eval_cdf(dst_right, y_upper) - tau_right) /
        (1 - tau_right) * (1 - tau_left) + tau_left
      res
    } else {
      stop("Not yet programmed.")
    }
  })
}

#' @export
eval_quantile.graft <- function(object, at, ...) {
  with(object[["components"]], {
    if (identical(base, "left")) {
      lower <- vapply(at <= tau_left, isTRUE,
        FUN.VALUE = logical(1)
      )
      upper <- vapply(at > tau_left, isTRUE,
        FUN.VALUE = logical(1)
      )
      p_lower <- at[lower]
      p_upper <- at[upper]
      res <- rep(NA_real_, length(at))
      res[lower] <- eval_quantile(dst_left, p_lower)
      res[upper] <- eval_quantile(
        dst_right,
        (p_upper - tau_left) /
          (1 - tau_left) *
          (1 - tau_right) +
          tau_right
      )
      res
    } else {
      stop("Not yet programmed.")
    }
  })
}

#' @export
eval_density.graft <- function(object, at) {
  if (variable(object) != "continuous") {
    return(NULL)
  }
  with(object[["components"]], {
    if (identical(base, "left")) {
      lower <- vapply(at <= sep_y, isTRUE, FUN.VALUE = logical(1))
      upper <- vapply(at > sep_y, isTRUE, FUN.VALUE = logical(1))
      y_lower <- at[lower]
      y_upper <- at[upper]
      res <- rep(NA_real_, length(at))
      res[lower] <- eval_density(dst_left, y_lower)
      res[upper] <- eval_density(dst_right, y_upper) /
        (1 - tau_right) * (1 - tau_left)
      res
    } else {
    }
  })
}

#' @export
evi.graft <- function(x, ...) {
  with(x[["components"]], {
    evi(dst_right)
  })
}


# Moment-based quantities may require integration - TBD

# Using .dst method for:
# - get_hazard
# - get_chf
# - get_survival
# - median
