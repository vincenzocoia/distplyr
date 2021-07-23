#' @export
Ops.dst <- function(e1, e2) {
  op <- .Generic[[1]]
  switch(op,
    `+` = {
      if (inherits(e1, "dst")) {
        make_dst_shift(e1, e2)
      } else {
        make_dst_shift(e2, e1)
      }
    },
    `-` = {
      if (missing(e2)) {
        make_dst_negative(e1)
      } else if (inherits(e1, "dst")) {
        make_dst_shift(e1, e2)
      } else {
        make_dst_shift(make_dst_negative(e2), e1)
      }
    },
    `*` = {
      if (inherits(e1, "dst")) {
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
      make_dst_scale(d, cnst)
    },
    `/` = {
      if (inherits(e1, "dst")) {
        make_dst_scale(e1, e2)
      } else {
        recp <- make_dst_inverse(e1)
        if (e2 == 1) {
          recp <- make_dst_scale(recp, e2)
        } else if (e2 < 0) {
          recp <- make_dst_scale(make_dst_negative(e1), -e2)
        } else {
          recp <- make_dst_scale(e1, e2)
        }
        recp
      }
    },
    stop("Not a valid Operation")
  )
}
