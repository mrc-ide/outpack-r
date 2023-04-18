cache <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  logging_init() # nocov
}
