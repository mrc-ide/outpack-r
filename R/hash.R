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


hash_validate_file <- function(path, expected) {
  hash_validate(rehash_file(path, expected), expected, squote(path))
}


hash_validate_data <- function(data, expected, name = deparse(substitute(x))) {
  hash_validate(rehash_data(data, expected), expected, name)
}


hash_validate <- function(found, expected, name) {
  if (found != expected) {
    stop(sprintf(
      "Hash of %s does not match:\n - expected: %s\n - found:    %s",
      name, expected, found))
  }
  invisible(found)
}


rehash_data <- function(data, expected) {
  algorithm <- hash_parse(expected)$algorithm
  hash_data(data, algorithm)
}


rehash_file <- function(data, expected) {
  algorithm <- hash_parse(expected)$algorithm
  hash_file(data, algorithm)
}
