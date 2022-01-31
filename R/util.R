`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


system_file <- function(...) {
  system.file(..., mustWork = TRUE)
}


outpack_file <- function(path) {
  system_file(path, package = "outpack")
}
