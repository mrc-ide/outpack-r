## A simple content-addressable file store; we'll use this for the
## out-of-working-copy storage of outpack packets.  The basic
## structure is similar to the git store, in that we store hashes
## split at their first byte.  However, there's no clever packing, and
## the content is stored as is (not in git's 'blob <content>|tree
## <content>' format), and we include an additional layer with the
## hash used.
file_store <- R6::R6Class(
  "file_store",
  cloneable = FALSE,
  public = list(
    path = NULL,

    initialize = function(path) {
      fs::dir_create(path)
      self$path <- path
      lockBinding("path", self)
    },

    filename = function(hash) {
      dat <- hash_parse(hash)
      file.path(self$path, dat$algorithm, substr(dat$value, 1, 2),
                substr(dat$value, 3, nchar(dat$value)))
    },

    ## TODO: bulk get, with overwrite control
    get = function(hash, dst) {
      src <- self$filename(hash)
      if (!file.exists(src)) {
        stop(sprintf("Hash '%s' not found in store", hash))
      }
      fs::dir_create(dirname(dst))
      fs::file_copy(src, dst)
      invisible(dst)
    },

    ## TODO: bulk set

    ## TODO: allow verification of hashes here (but what happens if
    ## the algorithm differs?)
    put = function(src, hash_algorithm = "sha256") {
      hash <- hash_file(src, hash_algorithm)
      dst <- self$filename(hash)
      if (!fs::file_exists(dst)) {
        fs::dir_create(dirname(dst))
        fs::file_copy(src, dst)
        fs::file_chmod(dst, "a-wx")
      }
      invisible(hash)
    },

    list = function() {
      files <- withr::with_dir(
        self$path,
        as.character(fs::dir_ls(recurse = 2, type = "file")))
      sub("/", "", sub("/", ":", files))
    }
  ))
