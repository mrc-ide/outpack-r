temp_file <- function() {
  path <- tempfile()
  withr::defer_parent(unlink(path, recursive = TRUE))
  path
}
