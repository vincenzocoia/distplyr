#' Convert Distributions in Ellipsis to List
#'
#' Flattens distributions placed in an ellipsis argument into a list,
#' so that the ellipsis can include distributions themselves and lists
#' of distributions.
#'
#' @param ... Distribution objects, possibly also lists of distributions.
#' @param na.rm Logical; remove NA entries? Note that NULL entries are
#' always removed.
#' @return A list of distributions contained in the `...`, with NULL
#' entries discarded. If no distributions are present, returns `list()`.
#' @details An error is thrown if, after discarding NULL entries,
#' `...` contains non-distributions. This function is essentially a
#' wrapper around `rlang::flatten()`.
#' @examples
#' d <- dst_norm(0, 1)
#' dots_to_dsts(d, list(d, d), NULL)
dots_to_dsts <- function(..., na.rm) {
  dsts <- rlang::list2(...)
  dsts <- rlang::flatten_if(dsts, vctrs::vec_is_list)
  nulls <- vapply(dsts, is.null, FUN.VALUE = logical(1L))
  if (na.rm) {
    nulls <- nulls | vapply(dsts, is.na, FUN.VALUE = logical(1L))
    acceptable_entry <- function(x) is_distribution(x) || is.na(x)
  } else {
    acceptable_entry <- is_distribution
  }
  dsts <- dsts[!nulls]
  not_all_dsts <- !all(vapply(
    dsts, acceptable_entry, FUN.VALUE = logical(1L)
  ))
  if (not_all_dsts) {
    stop("Ellipsis must contain distributions only.")
  }
  dsts
}
