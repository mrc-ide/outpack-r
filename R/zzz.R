cache <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  lgr::get_logger("outpack", reset = TRUE)$set_propagate(FALSE) # nocov
}
