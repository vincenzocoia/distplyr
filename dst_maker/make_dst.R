#' @export
make_dst_parametric <- function(dst, arg, type, ...) {
  file_name <- paste("R/dst_", dst, ".R", sep = "")
  file.create(file_name)
  file_conn <- file(file_name)
  total_str <- ""
  list_str <- ""
  # TODO: Try to simplify
  for (argument in arg) {
    total_str <- paste(total_str, ", ", argument, sep = "")
    list_str <- paste(list_str,
      ", ",
      argument, " = ", argument,
      sep = ""
    )
  }
  total_str <- substr(total_str, 3, nchar(total_str))
  list_str <- substr(list_str, 3, nchar(list_str))

  # create a string for each function with arguments
  pt <- ""
  for (argument in names(formals(paste("p", dst, sep = "")))) {
    pt <- paste(pt, ", ", argument, sep = "")
  }
  pt <- substr(pt, 6, nchar(pt))

  qt <- ""
  for (argument in names(formals(paste("q", dst, sep = "")))) {
    qt <- paste(qt, ", ", argument, sep = "")
  }
  qt <- substr(qt, 6, nchar(qt))

  rt <- ""
  for (argument in names(formals(paste("r", dst, sep = "")))) {
    rt <- paste(rt, ", ", argument, sep = "")
  }
  rt <- substr(rt, 6, nchar(rt))

  dt <- ""
  for (argument in names(formals(paste("d", dst, sep = "")))) {
    dt <- paste(dt, ", ", argument, sep = "")
  }
  dt <- substr(dt, 6, nchar(dt))

  write(c(
    "#' @export",
    paste("dst_", dst, "<- function(", total_str, ") {", sep = ""),
    paste("res <- list(", "parameters = list(", list_str, "))", sep = ""),
    paste("new_parametric(",
      "l = res, ",
      "variable = \"",
      type,
      "\",",
      "class = \"",
      dst, "\")",
      sep = ""
    ),
    paste("} \n"),
    "#' @export",
    paste("eval_cdf.", dst, "<- function(object, at, ...) {", sep = ""),
    paste("with(parameters(object), {"),
    paste("p",
      dst,
      "(at, ",
      pt,
      ", ...)",
      sep = ""
    ),
    paste("})} \n"),
    "#' @export",
    paste("eval_density.", dst, "<- function(object, at, ...) {", sep = ""),
    paste("with(parameters(object), {"),
    paste("d", dst, "(at, ", dt, ", ...)", sep = ""),
    paste("})} \n"),
    "#' @export",
    paste("eval_quantile.", dst, "<- function(object, at, ...) {", sep = ""),
    paste("with(parameters(object), {"),
    paste("q", dst, "(at, ", qt, ", ...)", sep = ""),
    paste("})} \n"),
    "#' @export",
    paste("realise.", dst, "<- function(object, at, ...) {", sep = ""),
    paste("with(parameters(object), {"),
    paste("r", dst, "(at, ", rt, ", ...)", sep = ""),
    paste("})} \n")
  ), file_conn)
  close(file_conn)
}



#' @export
make_dst <- function(dst, arg, ...) {
  file_name <- paste("R/dst_", dst, ".R", sep = "")
  file.create(file_name)
  file_conn <- file(file_name)

  write(c(
    "#' @export",
    paste("dst_", dst, "<- function( {", sep = ""),
    # Include Something,
    paste("} \n"),
    "#' @export",
    paste("eval_cdf.", dst, "<- function(object, at, ...) {", sep = ""),
    paste("p", dst, "(object, at, ...)", sep = ""),
    paste("} \n"),
    "#' @export",
    paste("eval_density.", dst, "<- function(object, at, ...) {", sep = ""),
    paste("d", dst, "(object, at, ...)", sep = ""),
    paste("} \n"),
    "#' @export",
    paste("eval_quantile.", dst, "<- function(object, at, ...) {", sep = ""),
    paste("p", dst, "(object, at, ...)", sep = ""),
    paste("} \n"),
    "#' @export",
    paste("realise.", dst, "<- function(object, at, ...) {", sep = ""),
    paste("p", dst, "(object, at, ...)", sep = ""),
    paste("} \n")
  ), file_conn)
  close(file_conn)
}