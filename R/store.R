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
      if (any(!file.exists(src))) {
        missing <- hash[!file.exists(src)]
        message <- sprintf("Hash not found in store:\n%s",
                           paste(sprintf("  - %s", missing), collapse = "\n"))
        stop(not_found_error(message, missing))
      }
      fs::dir_create(dirname(dst))
      fs::file_copy(src, dst)
      invisible(dst)
    },

    exists = function(hash) {
      file.exists(self$filename(hash))
    },

    ## TODO: bulk set
    ##
    ## TODO: allow computing hash here with nothing (hash = NULL),
    ## though that requires working out what the algorithm should be.
    ## Our current use knows the hash at the point of insertion and
    ## the validation is very useful!
    put = function(src, hash, move = FALSE) {
      hash_validate_file(src, hash)
      dst <- self$filename(hash)
      if (!fs::file_exists(dst)) {
        fs::dir_create(dirname(dst))
        if (move) {
          fs::file_move(src, dst)
        } else {
          fs::file_copy(src, dst)
        }
        fs::file_chmod(dst, "a-wx")
      } else if (move) {
        unlink(src)
      }
      invisible(hash)
    },

    list = function() {
      files <- with_dir(
        self$path,
        as.character(fs::dir_ls(recurse = 2, type = "file")))
      sub("/", "", sub("/", ":", files))
    },

    destroy = function() {
      fs::dir_delete(self$path)
    },

    tmp = function() {
      path <- file.path(self$path, "tmp")
      fs::dir_create(file.path(self$path, "tmp"))
      tempfile(tmpdir = path)
    }
  )
)
