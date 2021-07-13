
#' @export
prev_discrete <- function() {
  # t < -tidyselect::peek_vars()
  # UseMethod("prev_discrete", obj)
}

#' @export
prev_discrete.dst <- function() {
  make_empty_discontinuities_df()
}

#' @export
prev_discrete.pois <- function() {
  object <- parent.frame()$obj
  print(object)
}

#' @export
testFunc <- function(obj, todo) {
  tod <- rlang::enquo(todo)
  print(as.data.frame(obj[[1]]))
  rlang::eval_tidy(tod, data = as.data.frame(obj[[1]]))
}
