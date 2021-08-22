#' @export
Ops.gpd <- function(e1, e2) {
	op <- .Generic[[1]]
	switch(op,
		   `+` = {
		   	if (inherits(e1, "gpd")) {
		   		mutate_parameters(e1, location = location + e2)
		   	} else {
		   		mutate_parameters(e2, location = e1 + location)
		   	}
		   },
		   `-` = {
		   	if (inherits(e1, "gpd")) {
		   		mutate_parameters(e1, location = location - e1)
		   	} else {
		   		e1 + flip(e2)
		   	}
		   },
		   `*` = {
		   	if (inherits(e1, "gpd")) {
		   		mutate_parameters(e1, location = location * e2, scale = scale * e2)
		   	} else {
		   		mutate_parameters(e2, location = e1 * location, scale = e1 * scale)
		   	}
		   },
		   `/` = {
		   	if (inherits(e1, "gpd")) {
		   		mutate_parameters(e1, location = location / e2, scale = scale / e2)
		   	} else {
		   		e1 * invert(e2)
		   	}
		   },
		   stop("Operation not currently supported.")
	)
}
