#' Substitute a new distribution for the tail
#'
#' Constructs a pdist that is the original pdist up to some cutpoint (on the
#' x-axis), then continues with pdist2.
#'
#' @param pdist1,pdist2,qdist1,qdist2 Vectorized functions representing the lower (1)
#' and upper (2) distribution functions (pdist) and quantile functions (qdist).
#' @param sep_x Single numeric representing the value of the random
#' variable defining the separation point.
#'
#' @details Gives the following distribution: if W1 and W2 have
#' pdist's pdist1 and pdist2, let p = pdist1(sep_x), and let L be Bernoulli(p),
#' whose outcome indicates whether the lower distribution will be used or not.
#' Then the resulting pdist is that of W1 if L=1, and W2 if L=0.
#'
#' @return Named list of two components: pdist and qdist, which are vectorized
#' functions that preserve NA's.
#' @export
substail <- function(pdist1, pdist2, qdist1, qdist2, sep_x) {
    tau1 <- pdist1(sep_x)
    tau2 <- pdist2(sep_x)
    pdist <- function(x) {
        lower <- sapply(x <= sep_x, isTRUE)
        upper <- sapply(x > sep_x, isTRUE)
        x_lower <- x[lower]
        x_upper <- x[upper]
        res <- rep(NA_real_, length(x))
        res[lower] <- pdist1(x_lower)
        res[upper] <- (pdist2(x_upper) - tau2) / (1 - tau2) * (1 - tau1) + tau1
        res
    }
    qdist <- function(p) {
        lower <- sapply(p <= tau1, isTRUE)
        upper <- sapply(p > tau1, isTRUE)
        p_lower <- p[lower]
        p_upper <- p[upper]
        res <- rep(NA_real_, length(p))
        res[lower] <- qdist1(p_lower)
        res[upper] <- qdist2((p_upper - tau1) / (1 - tau1) * (1 - tau2) + tau2)
        res
    }
    list(pdist = as_pdist(pdist),
         qdist = as_qdist(qdist))
}
