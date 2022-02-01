`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


squote <- function(x) {
  sprintf("'%s'", x)
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
  # assert_file_exists(path)
  con <- file(path, open = "rb")
  on.exit(close(con))
  hash_data(con, algorithm)
}


hash_data <- function(data, algorithm) {
  assert_scalar_character(algorithm)
  value <- openssl::multihash(data, algorithm)[[algorithm]]
  sprintf("%s:%s", algorithm, as.character(value))
}


file_exists <- function(..., check_case = FALSE, workdir = NULL,
                        force_case_check = FALSE) {
  files <- c(...)
  if (!is.null(workdir)) {
    assert_scalar_character(workdir)
    owd <- setwd(workdir) # nolint
    on.exit(setwd(owd)) # nolint
  }
  exists <- file.exists(files)

  if (check_case) {
    incorrect_case <- logical(length(files))
    if (!is_linux() || force_case_check) {
      incorrect_case[exists] <-
        !vlapply(files[exists], file_has_canonical_case)
      if (any(incorrect_case)) {
        correct <- vcapply(files[incorrect_case], file_canonical_case)
        names(correct) <- files[incorrect_case]
        attr(exists, "incorrect_case") <- incorrect_case
        attr(exists, "correct_case") <- correct
        exists[incorrect_case] <- FALSE
      }
    }
  }

  exists
}


is_linux <- function() {
  tolower(Sys.info()[["sysname"]]) == "linux"
}


parse_hash <- function(hash) {
  re <- "^([[:alnum:]]+):([[:xdigit:]]+)$"
  stopifnot(all(grepl(re, hash))) # TODO: better error
  list(algorithm = sub(re, "\\1", hash),
       value = sub(re, "\\2", hash))
}


drop_class <- function(x) {
  class(x) <- NULL
  x
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
