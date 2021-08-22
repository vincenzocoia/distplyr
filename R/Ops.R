#' @export
Ops.dst <- function(e1, e2) {
  op <- .Generic[[1]]
  switch(op,
    `+` = {
      if (is_distribution(e1)) {
        shift(e1, e2)
      } else {
        shift(e2, e1)
      }
    },
    `-` = {
      if (missing(e2)) {
        make_dst_negative(e1)
      } else if (is_distribution(e1)) {
        shift(e1, e2)
      } else {
        shift(make_dst_negative(e2), e1)
      }
    },
    `*` = {
      if (is_distribution(e1)) {
        d <- e1
        cnst <- e2
      } else {
        d <- e2
        cnst <- e1
      }
      if (cnst < 0) {
        d <- make_dst_negative(d)
        cnst <- -cnst
      }
      if (cnst == 0) {
        return(dst_degenerate(0))
      }
      multiply(d, cnst)
    },
    `/` = {
      if (is_distribution(e1)) {
        multiply(e1, 1 / e2)
      } else {
        if (e1 == 0) {
          dst_degenerate(0)
        } else if (e1 < 0) {
          (-e1) * make_dst_inverse(make_dst_negative(e2))
        } else {
          e1 * make_dst_inverse(e2)
        }
      }
    },
    stop("Operation currently not supported.")
  )
}
