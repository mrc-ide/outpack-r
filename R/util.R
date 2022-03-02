`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}


vnapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, numeric(1), ...)
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


system_file <- function(...) {
  system.file(..., mustWork = TRUE)
}


outpack_file <- function(path) {
  system_file(path, package = "outpack")
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


## Designed for use reading json files as a single string and dropping
## and trailing whitespace
read_string <- function(path) {
  trimws(paste(readLines(path), collapse = "\n"))
}


## We could rewrite this non-recurively, this just comes from orderly
find_file_descend <- function(target, start = ".", limit = "/") {
  root <- normalizePath(limit, mustWork = TRUE)
  start <- normalizePath(start, mustWork = TRUE)

  f <- function(path) {
    if (file.exists(file.path(path, target))) {
      return(path)
    }
    if (normalizePath(path, mustWork = TRUE) == root) {
      return(NULL)
    }
    parent <- normalizePath(file.path(path, ".."))
    if (parent == path) {
      return(NULL)
    }
    Recall(parent)
  }
  ret <- f(start)
  if (!(is.null(ret))) {
    ret <- normalizePath(ret, mustWork = TRUE)
  }
  ret
}


max_time <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }
  max(x)
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


val_to_bytes <- function(x, nbytes) {
  n <- round((x %% 1) * 256 ^ nbytes)
  paste(packBits(intToBits(n))[nbytes:1], collapse = "")
}


## TODO: make sure we convert to UTC here
iso_time_str <- function(time = Sys.time()) {
  strftime(time, "%Y%m%d-%H%M%S")
}


str_iso_time <- function(str) {
  as.POSIXct(str, "UTC", "%Y%m%d-%H%M%S")
}


to_json <- function(x) {
  jsonlite::toJSON(x, pretty = FALSE, auto_unbox = FALSE,
                   json_verbatim = TRUE, na = "null", null = "null")
}
