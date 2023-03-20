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


val_to_bytes <- function(x, nbytes) {
  n <- round((x %% 1) * 256 ^ nbytes)
  paste(packBits(intToBits(n))[nbytes:1], collapse = "")
}


iso_time_str <- function(time = Sys.time()) {
  strftime(time, "%Y%m%d-%H%M%S", tz = "UTC")
}


time_to_num <- function(time = Sys.time()) {
  as.numeric(time)
}


num_to_time <- function(num) {
  as.POSIXct(num, origin = "1970-01-01", tz = "UTC")
}


empty_time <- function() {
  num_to_time(numeric(0))
}


to_json <- function(x, schema) {
  json <- jsonlite::toJSON(x, pretty = FALSE, auto_unbox = FALSE,
                           json_verbatim = TRUE, na = "null", null = "null")
  if (should_validate_schema(schema)) {
    outpack_schema(schema)$validate(json, error = TRUE)
  }
  json
}


from_json <- function(x, ...) {
  jsonlite::fromJSON(x, simplifyVector = FALSE, ...)
}


data_frame <- function(...) {
  ret <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  rownames(ret) <- NULL
  ret
}


squote <- function(x) {
  sprintf("'%s'", x)
}


with_dir <- function(path, code) {
  owd <- setwd(path) # nolint
  on.exit(setwd(owd)) # nolint
  force(code)
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


set_class <- function(x, cls) {
  class(x) <- cls
  x
}


is_call <- function(expr, valid) {
  is.recursive(expr) && as.character(expr[[1]]) %in% valid
}


last <- function(x) {
  x[[length(x)]]
}


deparse_str <- function(expr) {
  paste(deparse(expr), collapse = "\n")
}


source_script <- function(path, envir, echo) {
  source(path, local = envir, # nolint
         echo = echo, max.deparse.length = Inf)
}
