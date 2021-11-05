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
#' a <- distionary::dst_norm(0, 1)
#' b <- distionary::dst_norm(5, 2)
#' m1 <- mix(a, b, weights = c(1, 4))
#' #plot(m1)
#' distionary::variable(m1)
#'
#' c <- distionary::dst_empirical(0:6)
#' m2 <- mix(a, b, c, weights = c(0.2, 0.5, 0.3))
#' #plot(m2, n = 1001)
#' distionary::variable(m2)
#' @export
mix <- function(..., weights = 1, na.rm = FALSE) {
  dsts <- rlang::quos(...)
  dsts <- lapply(dsts, rlang::eval_tidy)
  not_all_dsts <- !all(vapply(
  	dsts, distionary::is_distribution, FUN.VALUE = logical(1L)
  ))
  if (not_all_dsts) {
    stop("Ellipsis must contain distributions only.")
  }
  n <- length(dsts)
  if (length(weights) == 1L) {
    weights <- rep(weights, n)
  }
  if (length(weights) != n) {
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
  if (length(probs) == 1L) {
    return(dsts[[1L]])
  }
  res <- list(
  	components = list(
  		distributions = dsts,
  		probs = probs
  	)
  )
  var_type <- vapply(dsts, distionary::variable, FUN.VALUE = character(1L))
  var_unique <- unique(var_type)
  if (length(var_unique) > 1L) {
    var_unique <- "mixed"
  }
  new_mix(res, variable = var_unique)
}

#' Constructor function for "mix" objects
#'
#' @param l List containing the components of a mixture distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#'   or "mixed".
#' @param ... Other attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_mix <- function(l, variable, ..., class = character()) {
	distionary::new_distribution(
		l, variable = variable, class = c(class, "mix")
	)
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

