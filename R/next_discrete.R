#' @export
next_discrete <- function(object, at, ...) UseMethod("next_discrete")

#' @export
next_discrete.dst <- function(object, at, ...) {
    if (variable(object) == "continuous") {
        NaN
    }
}

#' @export
next_discrete.pois <- function(object, at, ...) {
    # check if the number is a whole number
    if (at < 0) {
        new_at <- 0
    } else if (at %% 1 == 0) {
        new_at <- at + 1
    } else {
        new_at <- ceiling(at)
    }
    c(new_at)
}

#' @export
next_discrete.finite <- function(object, at, ...) {
    if (at >= max(object$probabilities$location)) {
        return(NaN)
    }
    next_point <- subset(object$probabilities, location > at, at = at)[1, ]
    if (is.null(next_point)) {
        return(NaN)
    }
    c(next_point[[1]])
}

#' @export
next_discrete.degenerate <- function(object, at, ...) {
    with(parameters(object), {
        if (at >= location) {
            NaN
        } else {
            c(location)
        }
    })
}

asda