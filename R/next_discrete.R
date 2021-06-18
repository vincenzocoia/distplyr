#' @export
next_discrete <- function(object, at, ...) UseMethod("next_discrete")

#' @export
next_discrete.norm <- function(object, at, ...) {
    # c()
    res <- data.frame()
    convert_dataframe_to_tibble(res)
}

#' @export
next_discrete.gpd <- function(object, at, ...) {
    # c()
    res <- data.frame()
    convert_dataframe_to_tibble(res)
}

#' @export
next_discrete.unif <- function(object, at, ...) {
    # c()
    res <- data.frame()
    convert_dataframe_to_tibble(res)
}

#' @export
next_discrete.pois <- function(object, at, ...) {
    # c()
    next_point <- c()
    res <- data.frame()
    convert_dataframe_to_tibble(res)
}