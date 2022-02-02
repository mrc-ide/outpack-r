`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
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


hash_file <- function(path, algorithm = "sha256") {
  assert_file_exists(path)
  con <- file(path, open = "rb")
  on.exit(close(con))
  hash_data(con, algorithm)
}


hash_data <- function(data, algorithm) {
  assert_scalar_character(algorithm)
  value <- openssl::multihash(data, algorithm)[[algorithm]]
  sprintf("%s:%s", algorithm, as.character(value))
}


parse_hash <- function(hash) {
  re <- "^([[:alnum:]]+):([[:xdigit:]]+)$"
  stopifnot(all(grepl(re, hash))) # TODO: better error
  list(algorithm = sub(re, "\\1", hash),
       value = sub(re, "\\2", hash))
}
