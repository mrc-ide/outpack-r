## This is not really meant for use; we'll need to derive a better set
## of primatives here later.  However, this might be enough to get
## us going.
test_outpack_run <- function(src, dst, name, script,
                             id = NULL, depends = NULL, root = NULL) {
  if (!is.null(depends)) {
    stop("WRITEME")
  }

  root <- outpack_root_locate(root)
  config <- root$config
  hash_algorithm <- root$config$hash_algorithm

  ## TODO: what do we want to assume here about paths? Importantly we
  ## cannot require that things are run from the outpack root
  ## directory (or the hashing and copying would fail poorly!) but we
  ## might allow running in place.

  ## TODO: We might optionally allow outputs to be listed here so that
  ## they can be excluded from the input hashing, and correctly found
  ## as outputs.  This disallows any orderly-like concept of
  ## inputs-that-are-outputs, but that's fine because we'll be less
  ## restrictive on that anyway.
  ##
  ## if (src == root$path) {
  ##   stop()
  ## }
  inputs <- dir(src, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  stopifnot(!file.exists(dst))
  fs::dir_create(dst)

  ## TODO: This is not going to work as expected for subdirectories
  fs::file_copy(file.path(src, inputs), dst)

  time_start <- Sys.time()
  withr::with_dir(dst, sys.source(script, envir = new.env()))
  time_end <- Sys.time()
  time <- list(start = time_start, end = time_end)

  contents <- dir(dst, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  outputs <- setdiff(contents, inputs)

  json <- outpack_metadata_create(dst, name, id, time, inputs, outputs,
                                  depends = depends)

  list(path = dst, json = json)
}
