hash_file <- function(path, algorithm = "sha256") {
  assert_file_exists(path)
  con <- file(path, open = "rb")
  on.exit(close(con))
  hash_data(con, algorithm)
}


## It might be useful here to allow a progress bar to cope with very
## large files
hash_dir <- function(path, algorithm = "sha256") {
  files <- dir(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  set_names(vcapply(file.path(path, files), hash_file), files)
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


hash_validate <- function(a, b) {

}
