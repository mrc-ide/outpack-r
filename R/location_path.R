outpack_location_path <- R6::R6Class(
  "outpack_location_path",

  private = list(
    root = NULL
  ),

  public = list(
    initialize = function(path) {
      ## TODO: filter so that we only count some locations as
      ## possibilities to export.
      private$root <- outpack_root_open(path, locate = FALSE)
    },

    list = function() {
      ## Later on, it might be possible to generalise this to allow us
      ## to request "changes since time" or similar, to reduce the
      ## total amount of information that travels across the wire.
      dat <- private$root$index()$location
      dat[c("packet", "time", "hash")]
    },

    metadata = function(packet_ids) {
      ## TODO: if we're filtering based on which location we're
      ## shipping results from, then we need to validate that these
      ## ids are all found within our data.
      dat <- private$root$index()$location
      msg <- setdiff(packet_ids, dat$packet)
      if (length(msg) > 0) {
        stop("Some packet ids not found: ",
             paste(squote(msg), collapse = ", "))
      }
      paths <- file.path(private$root$path, ".outpack", "metadata", packet_ids)
      ret <- vcapply(paths, read_string)
      names(ret) <- packet_ids
      ret
    },

    fetch_file = function(hash, dest) {
      ## TODO: we might need to give some better hints here as to what
      ## the user was looking for, but almost certainly that's better
      ## done by the calling function.
      if (private$root$config$core$use_file_store) {
        path <- private$root$files$filename(hash)
        if (!file.exists(path)) {
          stop(sprintf("Hash '%s' not found at location", hash))
        }
      } else {
        path <- find_file_by_hash(private$root, hash)
        if (is.null(path)) {
          stop(sprintf("Hash '%s' not found at location", hash))
        }
      }
      fs::file_copy(path, dest)
      dest
    },

    unknown_packets = function(ids, unpacked) {
      root_unknown_packets(ids, unpacked, private$root)
    },

    unknown_files = function(hashes) {
      root_unknown_files(hashes, private$root)
    }
  ))
