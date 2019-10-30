#' Concatenate two functions
#' 
#' Returns outer(inner(.)).
circ <- function(outer, inner) function(x) outer(inner(x))