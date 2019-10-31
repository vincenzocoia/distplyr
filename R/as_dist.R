# as_pdist <- function(x) {
#     subclass <- class(x)
#     if ("pdist" %in% subclass) return(x)
#     class(x) <- c("pdist", subclass)
#     x
# }
#
# as_qdist <- function(x) {
#     subclass <- class(x)
#     if ("qdist" %in% subclass) return(x)
#     class(x) <- c("qdist", subclass)
#     x
# }
#
# as_rdist <- function(x) {
#     subclass <- class(x)
#     if ("rdist" %in% subclass) return(x)
#     class(x) <- c("rdist", subclass)
#     x
# }
#
# as_rdist <- function(x) {
#     subclass <- class(x)
#     if ("ddist" %in% subclass) return(x)
#     class(x) <- c("ddist", subclass)
#     x
# }
