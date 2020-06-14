eval_kdf <- function(object, at, bw ="nrd0", kernel = "gaussian") {
	if (variable(object) != "discrete") {
		stop("Kernel density function is only ",
			 "calculated for discrete distributions.")
	}

}
