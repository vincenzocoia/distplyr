.onLoad <- function(libname, pkgname) {
  msg <- paste(
    "WARNING: you've installed distplyr from the wrong repository.",
    "Install it via `remotes::install_github('probaverse/distplyr')`."
  )
  packageStartupMessage(msg, domain = NULL, appendLF = TRUE)
}
