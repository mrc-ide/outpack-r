hash_file <- function(path, algorithm = "sha256") {
  assert_file_exists(path)
  con <- file(path, open = "rb")
  on.exit(close(con))
  hash_data(con, algorithm)
}


hash_files <- function(paths, algorithm = "sha256", named = FALSE) {
  vcapply(paths, hash_file, algorithm, USE.NAMES = named)
}


hash_data <- function(data, algorithm) {
  assert_scalar_character(algorithm)
  value <- openssl::multihash(data, algorithm)[[algorithm]]
  sprintf("%s:%s", algorithm, as.character(value))
}


hash_parse <- function(hash) {
  re <- "^([[:alnum:]]+):([[:xdigit:]]+)$"
  stopifnot(all(grepl(re, hash))) # TODO: better error
  list(algorithm = sub(re, "\\1", hash),
       value = sub(re, "\\2", hash))
}


hash_validate <- function(path, expected) {
  algorithm <- hash_parse(expected)$algorithm
  found <- hash_file(path, algorithm)
  if (found != expected) {
    stop(sprintf(
      "Hash of '%s' does not match:\n - expected: %s\n - found:    %s",
      path, expected, found))
  }
  invisible(found)
}


hash_validate_data <- function(data, expected, name = deparse(substitute(x))) {
  algorithm <- hash_parse(expected)$algorithm
  found <- hash_data(data, algorithm)
  if (found != expected) {
    stop(sprintf(
      "Hash of %s does not match:\n - expected: %s\n - found:    %s",
      name, expected, found))
  }
  invisible(found)
}
