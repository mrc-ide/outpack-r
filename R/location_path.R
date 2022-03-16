outpack_location_path <- R6::R6Class(
  "outpack_location_path",

  private = list(
    root = NULL
  ),

  public = list(
    initialize = function(path) {
      ## TODO: filter so that we only count some locations as
      ## possibilities to export.
      private$root <- outpack_root_open(path)
    },

    list = function() {
      ## Later on, it might be possible to generalise this to allow us
      ## to request "changes since time" or similar, to reduce the
      ## total amount of information that travels across the wire.
      dat <- private$root$index_update()$location
      dat[c("id", "time", "hash")]
    },

    metadata = function(ids) {
      ## TODO: if we're filtering based on which location we're
      ## shipping results from, then we need to validate that these
      ## ids are all found within our data.
      dat <- private$root$index_update()$location
      msg <- setdiff(ids, dat$id)
      if (length(msg) > 0) {
        stop("Some ids not found: ", paste(squote(msg), collapse = ", "))
      }
      paths <- file.path(private$root$path, ".outpack", "metadata", ids)
      ret <- vcapply(paths, read_string)
      names(ret) <- ids
      ret
    }
  ))
