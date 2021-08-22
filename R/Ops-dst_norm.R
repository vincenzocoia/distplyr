#' @export
Ops.norm <- function(e1, e2) {
	op <- .Generic[[1]]
	switch(op,
		   `+` = {
		   	if (is_distribution(e1)) {
		   		mutate_parameters(e1, mean = mean + e2)
		   	} else {
		   		mutate_parameters(e2, mean = e1 + mean)
		   	}
		   },
		   `-` = {
		   	if (missing(e2)) {
		   		mutate_parameters(e1, mean = -mean)
		   	} else if (is_distribution(e1)) {
		   		mutate_parameters(e1, mean = mean - e2)
		   	} else {
		   		mutate_parameters(e2, mean = e1 - mean)
		   	}
		   },
		   `*` = {
		   	if (is_distribution(e1)) {
		   		mutate_parameters(e1, mean = mean * e2, variance = variance * e2^2)
		   	} else {
		   		mutate_parameters(e2, mean = e1 * mean, variance = e1^2 * variance)
		   	}
		   },
		   `/` = {
		   	if (is_distribution(e1)) {
		   		mutate_parameters(e1, mean = mean / e2, variance = variance / e2^2)
		   	} else {
		   		e1 * make_dst_inverse(e2)
		   	}
		   },
		   stop("Operation not currently supported.")
	)
}
