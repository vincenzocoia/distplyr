#' Make a generalized Pareto distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param loc,scale,shape Parameters of the GPD.
#' @return Object of class "dst" of a GPD.
#' @export
dst_gpd <- function(loc, scale, shape) {
	if (scale == 0) return(dst_degen(loc))
	if (scale < 0) stop("'scale' parameter must be non-negative.")
	mu <- ifelse(shape < 1,
				 loc + scale / (1 - shape),
				 Inf)
	ss <- ifelse(shape < 1/2,
				 scale ^ 2 / (1 - shape) ^ 2 / (1 - 2 * shape),
				 Inf)
	sig <- sqrt(ss)
	if (shape == 0) {
		cdf <- function(x) {
			left <- x < loc
			z <- (x - loc) / scale
			res <- 1 - exp(-z)
			res[left] <- 0
			res
		}
		qf <- function(x) {
			invalid <- x < 0 | x > 1
			res <- loc - scale * log(1 - x)
			res[invalid] <- NaN
			res
		}
		pdf <- function(x) {
			outside <- x < loc
			z <- (x - loc) / scale
			res <- exp(-z) / scale
			res[outside] <- 0
			res
		}
	} else {
		if (shape > 0) {
			rightend <- Inf
		} else {
			rightend <- loc - scale / shape
		}
		cdf <- function(x) {
			left <- x < loc
			right <- x > rightend
			z <- (x - loc) / scale
			res <- 1 - (1 + shape * z) ^ (-1 / shape)
			res[left] <- 0
			res[right] <- 1
			res
		}
		qf <- function(x) {
			invalid <- x < 0 | x > 1
			t <- 1 / (1 - x)
			res <- loc + scale * (t ^ shape - 1) / shape
			res[invalid] <- NaN
			res
		}
		pdf <- function(x) {
			outside <- x < loc | x > rightend
			z <- (x - loc) / scale
			res <- (1 + shape * z) ^ (-1 / shape - 1) / scale
			res[outside] <- 0
			res
		}
	}
	rand <- function(n) qf(stats::runif(n))
	dst(fun_cumu = cdf,
		fun_quant = qf,
		fun_prob = pdf,
		fun_rand = rand,
		name = "GPD",
		param = c(loc = loc, scale = scale, shape = shape),
		prop = list(mean = mu,
					var  = ss,
					sd   = sig,
					evi  = shape,
					has_pdf = TRUE,
					has_pmf = FALSE))
}
