#' Mixture Distributions
#'
#' Create a mixture distribution.
#'
#' @param ... Distribution objects to mix.
#' @param weights Vector of weights corresponding to the distributions;
#' or, single numeric for equal weights.
#' @param na.rm Remove distributions corresponding to \code{NA} weights?
#' Default is \code{FALSE}.
#' @return A mixture distribution -- an empty distribution if any weights
#' are \code{NA} and `na.rm = FALSE`, the default.
#' @examples
#' a <- dst_norm(0, 1)
#' b <- dst_norm(5, 2)
#' m1 <- mix(a, b, weights = c(1, 4))
#' plot(m1)
#' variable(m1)
#'
#' c <- dst_empirical(0:6)
#' m2 <- mix(a, b, c, weights = c(0.2, 0.5, 0.3))
#' plot(m2, n = 1001)
#' variable(m2)
#' @export
mix <- function(..., weights = 1, na.rm = FALSE) {
  dsts <- rlang::list2(...)
  if (!all(vapply(dsts, is_distribution, FUN.VALUE = logical(1L)))) {
    stop("Ellipsis must contain distributions only.")
  }
  n <- length(dsts)
  if (identical(length(weights), 1L)) {
    weights <- rep(weights, n)
  }
  if (!identical(n, length(weights))) {
    stop("There must be one weight per distribution specified.")
  }
  if (any(weights < 0, na.rm = TRUE)) {
    stop("Weights must not be negative.")
  }
  probs <- weights / sum(weights, na.rm = TRUE)
  na_probs <- is.na(probs)
  if (any(na_probs)) {
    if (!na.rm) {
      return(NA)
    }
    probs <- probs[!na_probs]
    dsts <- dsts[!na_probs]
  }
  zero_probs <- probs == 0
  if (any(zero_probs)) {
    probs <- probs[!zero_probs]
    dsts <- dsts[!zero_probs]
  }
  if (identical(length(probs), 1L)) {
    return(dsts[[1L]])
  }
  already_mixtures <- vapply(dsts, is_mix, FUN.VALUE = logical(1L))
  if (any(already_mixtures)) {
    dsts_mixture <- dsts[already_mixtures]
    dsts_inner <- lapply(dsts_mixture, function(dst) {
      dst$components$distributions
    })
    dsts_flat <- unlist(dsts_inner, recursive = FALSE)
    probs_mixture_outer <- probs[already_mixtures]
    probs_mixture_inner <- lapply(dsts_mixture, function(dst) {
      dst$components$probs
    })
    probs_mixture_list <- mapply(
      `*`,
      probs_mixture_outer,
      probs_mixture_inner,
      SIMPLIFY = FALSE
    )
    probs_mixture_flat <- unlist(probs_mixture_list, recursive = FALSE)
    stopifnot(length(probs_mixture_flat) == length(dsts_flat))
    new_probs <- c(probs_mixture_flat, probs[!already_mixtures])
    new_dsts <- c(dsts_flat, dsts[!already_mixtures])
    return(rlang::exec(mix, !!!new_dsts, weights = new_probs, na.rm = na.rm))
  }
  if (all(vapply(dsts, is_finite_dst, FUN.VALUE = logical(1L)))) {
    prob_dfs <- lapply(dsts, `[[`, "probabilities")
    y_list <- lapply(prob_dfs, `[[`, "location")
    y <- c(y_list, recursive = TRUE)
    weight_list_original <- lapply(prob_dfs, `[[`, "size")
    weight_list_scaled <- mapply(`*`, weight_list_original, probs,
      SIMPLIFY = FALSE
    )
    weight_scaled <- c(weight_list_scaled, recursive = TRUE)
    new_prob_df <- aggregate_weights(y, weight_scaled, sum_to_one = FALSE)
    res <- new_finite(list(probabilities = new_prob_df), variable = "discrete")
    return(res)
  }
  res <- list(components = list(
    distributions = dsts,
    probs = probs
  ))
  var_type <- vapply(dsts, variable, FUN.VALUE = character(1L))
  var_unique <- unique(var_type)
  if (length(var_unique) > 1L) {
    var_unique <- "mixed"
  }
  new_mix(res, variable = var_unique)
}

#' Constructor function for `mix` objects
#' @inheritParams new_distribution
new_mix <- function(l, variable, ..., class = character()) {
  new_distribution(l, variable = variable, class = c(class, "mix"))
}

#' @param object Object to be tested
#' @rdname mix
#' @export
is_mix <- function(object) inherits(object, "mix")

#' @rdname mix
#' @export
is.mix <- function(object) inherits(object, "mix")


#' @export
print.mix <- function(x, ...) {
  cat("Mixture Distribution\n")
  cat("\nComponents: ")
  if (requireNamespace("tibble", quietly = TRUE)) {
    cat("\n")
    print(tibble::as_tibble(x$components))
  } else {
    cat(length(x$components$probs))
  }
}

#' @export
slice_right.mix <- function(object, breakpoint, include = TRUE, ...) {
  with(object$components, {
    keep <- vapply(distributions, prob_right, FUN.VALUE = numeric(1L),
                   of = breakpoint, inclusive = include) < 1
    if (all(!keep)) {
      stop("No such distribution exists: ",
           "cannot slice off entire distribution.")
    }
    kept_d <- distributions[keep]
    kept_p <- probs[keep] / sum(probs[keep])
    sliced_d <- lapply(kept_d, slice_right,
                       breakpoint = breakpoint, include = include)
    mix(!!!sliced_d, weights = kept_p)
  })
}

#' @export
slice_left.mix <- function(object, breakpoint, include = TRUE, ...) {
  with(object$components, {
    keep <- vapply(distributions, prob_left, FUN.VALUE = numeric(1L),
                   of = breakpoint, inclusive = include) < 1
    if (all(!keep)) {
      stop("No such distribution exists: ",
           "cannot slice off entire distribution.")
    }
    kept_d <- distributions[keep]
    kept_p <- probs[keep]
    sliced_d <- lapply(kept_d, slice_left,
                       breakpoint = breakpoint, include = include)
    mix(!!!sliced_d, weights = kept_p)
  })
}

#' @export
mean.mix <- function(x, ...) {
  with(x[["components"]], {
    means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
    sum(probs * means)
  })
}

#' @export
variance.mix <- function(x, ...) {
  overall_mean <- mean(x)
  with(x[["components"]], {
    means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
    variances <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
    sum(probs * (variances + means^2 - overall_mean^2))
  })
}

#' @export
skewness.mix <- function(x, ...) {
  overall_mean <- mean(x)
  overall_sd <- stdev(x)
  with(x[["components"]], {
    means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
    vars <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
    sds <- sqrt(vars)
    skews <- vapply(distributions, skewness, FUN.VALUE = numeric(1L))
    cmoms <- list(
      zero = 1,
      first = 0,
      second = vars,
      third = skews * sds^3
    )
    terms <- lapply(0:3, function(k) {
      choose(3, k) * (means - overall_mean)^(3 - k) * cmoms[[k + 1L]]
    })
    sum(probs * Reduce(`+`, terms)) / overall_sd^3
  })
}

#' @export
kurtosis_exc.mix <- function(x, ...) {
  overall_mean <- mean(x)
  overall_var <- variance(x)
  with(x[["components"]], {
    means <- vapply(distributions, mean, FUN.VALUE = numeric(1L))
    vars <- vapply(distributions, variance, FUN.VALUE = numeric(1L))
    sds <- sqrt(vars)
    skews <- vapply(distributions, skewness, FUN.VALUE = numeric(1L))
    kurts <- vapply(distributions, kurtosis_raw, FUN.VALUE = numeric(1L))
    cmoms <- list(
      zero = 1,
      first = 0,
      second = vars,
      third = skews * sds^3,
      fourth = vars^2 * kurts
    )
    terms <- lapply(0:4, function(k) {
      choose(4, k) * (means - overall_mean)^(4 - k) * cmoms[[k + 1L]]
    })
    sum(probs * Reduce(`+`, terms)) / overall_var^2 - 3
  })
}

#' @export
eval_density.mix <- function(object, at, strict = TRUE) {
  if (variable(object) != "continuous") {
    return(NULL)
  }
  with(object[["components"]], {
    density_vals <- lapply(distributions, eval_density, at = at)
    p_times_f <- mapply(function(p, f) p * f, probs, density_vals,
      SIMPLIFY = FALSE
    )
    Reduce(`+`, p_times_f)
  })
}

#' @export
eval_cdf.mix <- function(object, at) {
  with(object[["components"]], {
    cdf_vals <- lapply(distributions, eval_cdf, at = at)
    p_times_cdfs <- mapply(function(p, f) p * f, probs, cdf_vals,
      SIMPLIFY = FALSE
    )
    Reduce(`+`, p_times_cdfs)
  })
}

#' @export
eval_quantile.mix <- function(object, at, tol = 1e-6, maxiter = 1000, ...) {
  distributions <- object[["components"]][["distributions"]]
  cdf <- get_cdf(object)
  discon <- discontinuities(object)
  res <- at
  ones <- vapply(at == 1, isTRUE, FUN.VALUE = logical(1L))
  if (any(ones)) {
    right_ends <- lapply(distributions, eval_quantile, at = 1)
    res[ones] <- do.call(max, right_ends)
  }
  res[!ones] <- eval_quantile_from_cdf(
    cdf, discon,
    at = at[!ones], tol = tol, maxiter = maxiter
  )
  res
}

#' @export
realise.mix <- function(object, n = 1, ...) {
  with(object[["components"]], {
    if (n == 0) {
      if (identical(variable(object), "categorical")) {
        return(character())
      } else {
        return(numeric())
      }
    }
    k <- length(distributions)
    id <- sample(1:k, size = n, replace = TRUE, prob = probs)
    sapply(id, function(i) realise(distributions[[i]]))
  })
}

#' @export
evi.mix <- function(x, ...) {
  if (is_finite_dst(x)) {
    return(NaN)
  }
  with(x[["components"]], {
    right_ends <- vapply(distributions, eval_quantile,
      at = 1,
      FUN.VALUE = numeric(1L)
    )
    max_end <- max(right_ends)
    has_max_ends <- right_ends == max_end
    evis <- vapply(distributions, evi, FUN.VALUE = numeric(1L))
    final_sign <- if (max_end < Inf) -1 else 1
    final_sign * max(abs(evis[has_max_ends]))
  })
}

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


#' @export
range.mix <- function(object, ...) {
  r <- lapply(object$components$distributions, range)
  low <- Reduce(min, r)
  high <- Reduce(max, r)
  c(low, high)
}

#' @export
eval_pmf.mix <- function(object, at, strict = TRUE, ...) {
  with(object$components, {
    cumulitive_sum <- 0
    for (i in 1:length(distributions)) {
      tryCatch(
        {
          cumlative_sum <- cumulitive_sum +
            eval_pmf(distributions[[i]], at, strict = strict) * probs[[i]]
        },
        error = function(c) {
          warning("A component distribution doesn't have a pmf. Perhaps you want to evaluate in non-strict mode?")
          return(NA)
        }
      )
    }
    cumulitive_sum
  })
}
