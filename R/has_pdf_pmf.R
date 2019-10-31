#' Check if a distribution has a pmf or pdf
#'
#' @param object Object of class "dst"
#' @return Logical value
#' @rdname has_d
#' @export
has_pmf <- function(object) UseMethod("has_pmf")

#' @rdname has_d
#' @export
has_pdf <- function(object) UseMethod("has_pdf")

#' @export
has_pmf.dst <- function(object) object$prop$has_pmf

#' @export
has_pdf.dst <- function(object) object$prop$has_pdf
