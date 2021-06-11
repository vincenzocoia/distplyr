#' Graft Distributions
#' Replace the tail of a distribution
# #' #' \code{graft_right()} keeps the left cdf unchanged to the left of
# #' #' sep_y, and makes a continuous connection with the right cdf
# #' #' (rescaled as appropriate).
# #' #' \code{graft_left()} keeps the right cdf unchanged to the right of
# #' #' sep_y, and makes a continuous connection with the left cdf
# #' #' (rescaled as appropriate),
# #' #' @param dst_left,dst_right Distributions to connect
# #' #' @param sep_y Value on the domain of the cdf to connect at.
# #' #' @return A grafted distribution object.
# #' #' @examples
# #' #' require(datasets)
# #' #' base <- dst_empirical(mpg, data = mtcars)
# #' #' right <- dst_gpd(25, 5, 1)
# #' #' g <- graft_right(base, right, sep_y = 25)
# #' #' plot(g, "cdf", n = 1001, to = 34)
# #' #' plot(base, "cdf", n = 1001, lty = 2, add = TRUE)
# #' #' @rdname graft
# #' #' @export
# graft_right <- function(dst_left, dst_right, sep_y) {
#   tau_left <- eval_cdf(dst_left, sep_y)
#   tau_right <- eval_cdf(dst_right, sep_y)
#   steps_left <- discontinuities(dst_left)
#   steps_left <- steps_left[steps_left[["location"]] <= sep_y, ]
#   steps_right <- discontinuities(dst_right)
#   steps_right <- steps_right[steps_right[["location"]] > sep_y, ]
#   steps_right[["size"]] <- steps_right[["size"]] * (1 - tau_left) /
#     (1 - tau_right)
#   steps_combined <- rbind(steps_left, steps_right)
#   # stopifnot(is_discontinuities_df(steps_combined))
#   v <- discontinuities_to_variable(steps_combined)
#   res <- list(
#     name = "Graft",
#     discontinuities = steps_combined,
#     components = list(
#       dst_left = dst_left,
#       dst_right = dst_right,
#       tau_left = tau_left,
#       tau_right = tau_right,
#       sep_y = sep_y,
#       base = "left"
#     )
#   )
#   if (identical(v, "discrete")) {
#     new_finite(res, variable = v, class = "graft")
#   } else {
#     new_distribution(res, variable = v, class = "graft")
#   }
# }
# #'
# #' #' @export
# print.graft <- function(x, ...) {
#   cat("Graft Distribution\n")
#   cat("\nBase: ")
#   cat(name(x[["components"]][["dst_left"]]), "Distribution")
#   cat("\n(Right) Tail: ")
#   cat(name(x[["components"]][["dst_right"]]), "Distribution")
#   cat("\nSeparated at outcome: ")
#   cat(x[["components"]][["sep_y"]])
#   cat("\n")
#   cat("\nNumber of Discontinuities: ", nrow(discontinuities(x)))
# }
# #'
# #'
# #' #' @param object Object to be tested
# #' #' @rdname graft
# #' #' @export
# #' is_graft <- function(object) inherits(object, "graft")
# #'
# #' #' @rdname graft
# #' #' @export
# #' is.graft <- function(object) inherits(object, "graft")
# #'
# #' #' @export
# eval_cdf.graft <- function(object, at) {
#   with(object[["components"]], {
#     if (identical(base, "left")) {
#       lower <- vapply(at <= sep_y, isTRUE, FUN.VALUE = logical(1))
#       upper <- vapply(at > sep_y, isTRUE, FUN.VALUE = logical(1))
#       y_lower <- at[lower]
#       y_upper <- at[upper]
#       res <- rep(NA_real_, length(at))
#       res[lower] <- eval_cdf(dst_left, y_lower)
#       res[upper] <- (eval_cdf(dst_right, y_upper) - tau_right) /
#         (1 - tau_right) * (1 - tau_left) + tau_left
#       res
#     } else {
#       stop("Not yet programmed.")
#     }
#   })
# }
# #'
# #' #' @export
# eval_quantile.graft <- function(object, at, ...) {
#   with(object[["components"]], {
#     if (identical(base, "left")) {
#       lower <- vapply(at <= tau_left, isTRUE,
#         FUN.VALUE = logical(1)
#       )
#       upper <- vapply(at > tau_left, isTRUE,
#         FUN.VALUE = logical(1)
#       )
#       p_lower <- at[lower]
#       p_upper <- at[upper]
#       res <- rep(NA_real_, length(at))
#       res[lower] <- eval_quantile(dst_left, p_lower)
#       res[upper] <- eval_quantile(
#         dst_right,
#         (p_upper - tau_left) /
#           (1 - tau_left) *
#           (1 - tau_right) +
#           tau_right
#       )
#       res
#     } else {
#       stop("Not yet programmed.")
#     }
#   })
# }
# #'
# #' #' @export
# eval_density.graft <- function(object, at) {
#   if (variable(object) != "continuous") {
#     return(NULL)
#   }
#   with(object[["components"]], {
#     if (identical(base, "left")) {
#       lower <- vapply(at <= sep_y, isTRUE, FUN.VALUE = logical(1))
#       upper <- vapply(at > sep_y, isTRUE, FUN.VALUE = logical(1))
#       y_lower <- at[lower]
#       y_upper <- at[upper]
#       res <- rep(NA_real_, length(at))
#       res[lower] <- eval_density(dst_left, y_lower)
#       res[upper] <- eval_density(dst_right, y_upper) /
#         (1 - tau_right) * (1 - tau_left)
#       res
#     } else {
#     }
#   })
# }
# #'
# #' #' @export
# evi.graft <- function(x, ...) {
#   with(x[["components"]], {
#     evi(dst_right)
#   })
# }

# #'
# #' # Moment-based quantities may require integration - TBD
# #'
# #' # Using .dst method for:
# #' # - get_hazard
# #' # - get_chf
# #' # - get_survival
# #' # - median
# #'

#' @export
graft_right <- function(object, dst_right, at, include_at = TRUE, ...) {
  if (!is_graft(object)) {
    graft(
      object,
      NULL,
      dst_right,
      NULL,
      at,
      NULL,
      include_at,
      dst_right,
      NULL
    )
  } else if (is.null(object$components$dst_left) != TRUE) {
    graft(
      object$component$base,
      object$component$dst_left,
      dst_right,
      object$component$left_at,
      at,
      object$component$include_at_left_in_base,
      include_at,
      dst_right,
      dst_left
    )
  } else if (is.null(object$components$dst_left) == TRUE) {
    graft(
      object$component$base,
      NULL,
      dst_right,
      NULL,
      at,
      NULL,
      include_at,
      dst_right,
      NULL
    )
  }
}

#' @export
graft_left <- function(object, dst_left, at, include_at = TRUE, ...) {
  if (!is_graft(object)) {
    graft(
      object,
      dst_left,
      NULL,
      at,
      NULL,
      include_at,
      NULL,
      dst_left,
      NULL
    )
  } else if (is.null(object$components$dst_right) != TRUE) {
    graft(
      object$component$base,
      dst_left,
      object$component$dst_right,
      at,
      object$component$right_at,
      include_at,
      dst_left,
      dst_right
    )
  } else if (is.null(object$components$dst_right) == TRUE) {
    graft(
      object$component$base,
      dst_left,
      NULL,
      at,
      NULL,
      include_at,
      NULL,
      dst_left,
      NULL
    )
  }
}

graft <- function(base, dst_left, dst_right, left_at, right_at, include_at_left_in_base, include_at_right_in_base, graft_side, other_side) {
  res <- list(
    name = "Graft",
    components = list(
      base = base,
      dst_left = dst_left,
      dst_right = dst_right,
      left_at = left_at,
      right_at = right_at,
      include_at_left_in_base = include_at_left_in_base,
      include_at_right_in_base = include_at_right_in_base
    )
  )
  if (is_finite_dst(graft_side) && is_finite_dst(res$component$base)) {
    temp_graft <- new_distribution(res, variable = "discrete", class = "graft")
    discont <- discontinuities(temp_graft)
    # discont needs to
    return(dst_finite(location, size, data = discont))
  }
  if (variable(graft_side) == "discrete" &
    variable(res$components$base) == "discrete") {
    if (is.null(other_side) || variable(other_side) == "discrete") {
      new_distribution(res, variable = "discrete", class = "graft")
    } else {
      new_distribution(res, variable = "mixed", class = "graft")
    }
  } else if (variable(graft_side) == "continuous" &
    variable(res$components$base) == "continuous") {
    if (is.null(other_side)) {
      new_distribution(res, variable = "continuous", class = "graft")
    } else if (variable(other_side) == "continuous") {
      new_distribution(res, variable = "continuous", class = "graft")
    } else {
      new_distribution(res, variable = "mixed", class = "graft")
    }
  }
  else {
    new_distribution(res, variable = "mixed", class = "graft")
  }
}


graft_right_new <- function(base, graft, breakpoint, include_breakpoint_in_base) {
  res <- list(
    components = list(
      base = base,
      graft = graft,
      breakpoint = breakpoint,
      include_breakpoint_in_base = include_breakpoint_in_base
    )
  )
  max_val <- range(base)
  if (breakpoint > max_val) {
    return(base)
  }
  if (is_finite_dst(base) && is_finite_dst(graft)) {
    temp_graft <- new_distribution(res, variable = "discrete", class = "graft_right")
    discont <- discontinuities(temp_graft)
    # discont needs to
    return(dst_finite(location, size, data = discont))
  }
  if (variable(graft) == "discrete" && variable(base) == "discrete") {
    new_distribution(res, variable = "discrete", class = "graft")
  } else if (variable(graft_side) == "continuous" &
    variable(res$components$base) == "continuous") {
    if (is.null(other_side)) {
      new_distribution(res, variable = "continuous", class = "graft")
    } else if (variable(other_side) == "continuous") {
      new_distribution(res, variable = "continuous", class = "graft")
    } else {
      new_distribution(res, variable = "mixed", class = "graft")
    }
  }
  else {
    new_distribution(res, variable = "mixed", class = "graft")
  }
}


#' @export
discontinuities.graft <- function() {

}

#' @export
print.graft <- function(x, ...) {
  cat("Graft Distribution\n")
  cat("\nComponents: ")
  if (requireNamespace("tibble", quietly = TRUE)) {
    cat("\n")
    cat("Base:")
    print(x$components$base)
    cat("Left At: ")
    print(x$components$left_at)
    cat("Left: ")
    print(x$components$dst_left)
    cat("Right At: ")
    print(x$components$right_at)
    cat("Right: ")
    print(x$components$dst_right)
  } else {
    cat(length(x$components$probs))
  }
}

#' @export
eval_cdf.graft <- function(object, at) {
  with(object[["components"]], {
    if (is.null(dst_left)) {
      # eval_cdf_at_right(object, at)
      eval_cdf_at_right(object, at)
    } else if ((is.null(dst_right))) {
      eval_cdf_at_left(object, at)
    } else {
      eval_cdf_both(object, at)
    }
  })
}

eval_cdf_at_right <- function(object, at) {
  with(object[["components"]], {
    if (at < right_at) {
      eval_cdf(base, at)
    } else if (at == right_at) {
      if (include_at_right_in_base) {
        eval_cdf(base, right_at)
      } else {
        dist_base <- discontinuities(base, right_at, right_at)
        dist_graft <- discontinuities(dst_right, right_at, right_at)
        # TODO: Need to transform dist_graft;
        # Or ignore since we are transforming anyways?
        if (length(dist_base[["location"]]) != 0 &
          length(dist_graft[["location"]]) != 0) {
          eval_cdf(base, right_at) -
            dist_base[["size"]][[1]] +
            dist_graft[["size"]][[1]]
        } else if (length(dist_base[["location"]]) == 0 &
          length(dist_graft[["location"]]) != 0) {
          eval_cdf(base, right_at) + dist_graft[["size"]][[1]]
        } else if (length(dist_base[["location"]]) != 0 &
          length(dist_graft[["location"]]) == 0) {
          eval_cdf(base, right_at) + dist_base[["size"]][[1]]
        } else {
          eval_cdf(base, right_at)
        }
      }
    } else if (at > right_at) {
      if (include_at_right_in_base) {
        apply_transformation_right(object, at, "base")
      } else {
        apply_transformation_right(object, at, "graft")
      }
    }
    # else if (at >= right_at & !include_at_right_in_base) {
    #   apply_transformation_right(object, at, "base")
    # } else if (at >= right_at & include_at_right_in_base) {
    #   apply_transformation_right(object, at, "graft")
    # }
  })
}

apply_transformation_right <- function(object, at, remote_point) {
  with(object[["components"]], {
    # p1
    base_cdf <- eval_cdf(
      base,
      right_at
    )
    # p2
    dst_right_cdf <- eval_cdf(
      base,
      right_at
    )
    # F(x)
    dst_to_at <- eval_cdf(
      dst_right,
      at
    )
    # if (remote_point == "graft") {
    #   dist <- discontinuities(dst_right, right_at, right_at)
    #   if (length(dist[["location"]]) != 0) {
    #     dst_right_cdf <- dst_right_cdf - dist[["size"]][[1]]
    #   }
    # }
    res <- (dst_to_at - dst_right_cdf) / (1 - dst_right_cdf)
    res <- (res * (1 - base_cdf)) + base_cdf
    if (remote_point == "base") {
      dist <- discontinuities(base, right_at, right_at)
      if (length(dist[["location"]]) != 0) {
        res <- res - dist[["size"]][[1]]
      }
    }
    res
  })
}


eval_cdf_at_left <- function(object, at) {
  # copied
  with(object[["components"]], {
    if (at <= right_at & include_at_right_in_base) {
      eval_cdf(base, at)
    } else if (at <= right_at & !include_at_right_in_base) {
      dist <- discontinuities(base, at, at)
      # What about mixed dist? Since there can be a dist at point and
      # subtracting one is removes from CDF? Subtract dist?
      if (length(dist[["location"]]) != 0) {
        eval_cdf(base, at) - dist[["size"]][[1]]
      } else {
        eval_cdf(base, at)
      }
    } else if (at >= right_at & !include_at_right_in_base) {
      apply_transformation_right(object, at, "base")
    } else if (at >= right_at & include_at_right_in_base) {
      apply_transformation_right(object, at, "graft")
    }
  })
}

eval_cdf_both <- function(object, at) {

}

#' @export
range.graft <- function() {

}

#' @export
eval_density.graft <- function(object, at) {
  if (is.null(object[["components"]][["dst_left"]])) {
    if (at >= object[["components"]][["right_at"]] &
      object[["components"]][["include_at_right_in_base"]]) {
      eval_density(object[["components"]][["base"]], at)
    } else if (at >= object[["components"]][["right_at"]] &
      !object[["components"]][["include_at_right_in_base"]]) {
      # figure out a way

      eval_density_at_point(object, at, "right", "right_at")
    }
  } else if (is.null(object[["components"]][["dst_right"]])) {
    if (at >= object[["components"]][["left_at"]] &
      object[["components"]][["include_at_right_in_base"]]) {
      eval_density(object[["components"]][["base"]], at)
    } else if (at >= object[["components"]][["left_at"]] &
      !object[["components"]][["include_at_right_in_base"]]) {
      # figure out a way

      eval_density_at_point(object, at, "left", "left_at")
    }
  }
}

eval_density_at_point <- function(object, at, side, at_point) {
  base_cdf <- eval_cdf(
    object[["components"]][["base"]],
    object[["components"]][[at_point]]
  )
  graft_at_point_cdf <- eval_cdf(
    object[["components"]][[side]],
    object[["components"]][[at_point]]
  )
  graft_at_pdf <- eval_density(
    object[["components"]][[side]],
    at
  )

  ((1 - base_cdf) / (1 - graft_at_point_cdf)) * graft_at_pdf
}


#' @param object Object to be tested
#' @rdname graft
#' @export
is_graft <- function(object) inherits(object, "graft")

#' @export
eval_quantile.graft <- function(x, at, ...) {
  # what if equal to breakpoint?
  with(x[["components"]], {
    if (at < left_at) {
      eval_cdf_at_left(object, at)
    } else if (at > right_at) {
      eval_quantile_at_right(object, at)
    } else if (at > left_at &
      at < right_at) {
      left_quantile <- eval_quantile_at_left(dst_left, left_at)
      base_quantile <- eval_quantile(base, at) -
        eval_quantile(base, left_at)
      left_quantile + base_quantile
    }
  })
}

eval_quantile_at_left <- function(object, at) {
  # cdf lower base
  base_cdf <- eval_cdf(
    object[["components"]][["base"]],
    object[["components"]][["left_at"]]
  )
  # cdf left
  graft_at_point_cdf <- eval_cdf(
    object[["components"]][["dst_right"]],
    object[["components"]][["left_at"]]
  )

  graft_at_pdf <- eval_density(
    object[["components"]][["dst_right"]],
    at
  )
  formula <- (graft_at_point_cdf / base_cdf) * at
  eval_quantile(object[["components"]][["dst_left"]], formula)
}

eval_quantile_at_right <- function(object, at) {
  base_cdf <- eval_cdf(
    object[["components"]][["base"]],
    object[["components"]][["right_at"]]
  )
  graft_at_point_cdf <- eval_cdf(
    object[["components"]][["dst_right"]],
    object[["components"]][["right_at"]]
  )
  formula <- ((at - base_cdf) / (1 - base_cdf)) *
    (1 - graft_at_point_cdf)
  eval_quantile(object[["components"]][["dst_right"]], formula)
}

#' @export
evi.graft <- function(x, ...) {
  with(x[["components"]], {
    evi(dst_right)
  })
}