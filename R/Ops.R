#' @export
Ops.dst <- function(e1, e2) {
  op <- .Generic[[1]]
  switch(op,
    `+` = {
      if (distionary::is_distribution(e1)) {
        shift(e1, e2)
      } else {
        shift(e2, e1)
      }
    },
    `-` = {
      if (missing(e2)) {
        flip(e1)
      } else if (distionary::is_distribution(e1)) {
        shift(e1, -e2)
      } else {
        shift(flip(e2), e1)
      }
    },
    `*` = {
      if (distionary::is_distribution(e1)) {
        d <- e1
        cnst <- e2
      } else {
        d <- e2
        cnst <- e1
      }
      multiply(d, cnst)
    },
    `/` = {
      if (distionary::is_distribution(e1)) {
        multiply(e1, 1 / e2)
      } else {
        d <- e2
        cnst <- e1
        if (cnst == 0) {
        	distionary::dst_degenerate(0)
        } else if (cnst < 0) {
          invert(flip(multiply(d, -cnst)))
        } else {
          invert(multiply(d, 1 / cnst))
        }
      }
    },
    stop("Operation currently not supported.")
  )
}
