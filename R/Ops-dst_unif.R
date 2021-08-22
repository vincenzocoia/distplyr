#' @export
Ops.unif <- function(e1, e2) {
	op <- .Generic[[1]]
	switch(op,
		   `+` = {
		   	if (inherits(e1, "unif")) {
		   		mutate_parameters(e1, min = min + e2, max = max + e2)
		   	} else {
		   		mutate_parameters(e2, min = min + e1, max = max + e1)
		   	}
		   },
		   `-` = {
		   	if (missing(e2)) {
		   		mutate_parameters(e1, min = -max, max = -min)
		   	} else if (inherits(e1, "unif")) {
		   		mutate_parameters(e1, min = min - e2, max = max - e2)
		   	} else {
		   		mutate_parameters(e2, min = e1 - max, max = e1 - min)
		   	}
		   },
		   `*` = {
		   	if (inherits(e1, "unif")) {
		   		d <- e1
		   		cnst <- e2
		   	} else {
		   		d <- e2
		   		cnst <- e1
		   	}
		   	if (cnst < 0) {
		   		return(-cnst * (-d))
		   	}
		   	mutate_parameters(e1, min = min * cnst, max = max * cnst)
		   },
		   `/` = {
		   	if (inherits(e1, "unif")) {
		   		mutate_parameters(e1, min = min / e2, max = max / e2)
		   	} else {
		   		make_dst_inverse(e2) * e1
		   	}
		   },
		   stop("Operation currently not supported.")
	)
}
