#' #' Distribution of an Order Statistic
#' #'
#' #' Distribution of the `k`th largest observation from an
#' #' independent draw of `n` observations from a distribution.
#' #'
#' #' @param distribution Distribution
#' #' @param k Positive integer indicating the position of the order statistic.
#' #' @param n Positive integer indicating the number of independent draws from
#' #' `distribution`.
#' #' @seealso `maximise()`
#' order_k <- function(distribution, k, n) {
#'   k_int <- as.integer(round(k))
#'   n_int <- as.integer(round(n))
#'   if (abs(k_int - k) > 0) {
#'     warning("Rounding `k` to the nearest integer.")
#'   }
#'   if (abs(n_int - n) > 0) {
#'     warning("Rounding `n` to the nearest integer.")
#'   }
#'   l <- list(distribution = distribution,
#'             k = k,
#'             n = n)
#'   new_orderstat(l, variable = variable(distribution))
#' }
#'
#' new_orderstat <- function(l, variable, ..., class = character()) {
#'   distionary::new_distribution(
#'     l, variable = variable, ..., class = c(class, "orderstat")
#'   )
#' }
