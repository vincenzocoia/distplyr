#' Check if a distribution has a pmf/pdf
#'
#' @param object Object of class "dst"
#' @return Logical value
#' @rdname has_d
#' @export
has_pmf <- function(...) UseMethod("has_pmf")

#' @rdname has_d
#' @export
has_pdf <- function(...) UseMethod("has_pdf")


has_pmf.dst <- function(object) object$prop$has_pmf
has_pdf.dst <- function(object) object$prop$has_pdf
