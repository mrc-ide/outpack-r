## This is not really meant for use; we'll need to derive a better set
## of primatives here later.  However, this might be enough to get
## us going.

## TODO: note that things like parameters etc are not pulled through
## here.

test_outpack_run <- function(src, dst, name, script,
                             id = NULL, depends = NULL, root = NULL) {
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

  fs::dir_copy(src, dst)

  withr::with_dir(dst, sys.source(script, envir = new.env()))

  json <- outpack_metadata_create(dst, name, id, depends = depends)

  list(path = dst, json = json)
}
