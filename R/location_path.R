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
    },

    push_file = function(src, hash) {
      location_path_import_file(src, hash, private$root)
    },

    push_metadata = function(packet_id, root) {
      ## TODO: make this easier to do:
      index <- root$index()$location
      hash <- index$hash[index$packet == packet_id][[1]]
      path <- file.path(root$path, ".outpack", "metadata", packet_id)
      location_path_import_metadata(read_string(path), hash, private$root)
    }
  ))


## This split just acts to make the http one easier to think about -
## it's not the job of the driver to do validation, but the server.
location_path_import_metadata <- function(str, hash, root) {
  dat <- outpack_metadata_load(as_json(str))
  id <- dat$id
  hash_validate_data(str, hash, sprintf("metadata for '%s'", id))

  if (length(unknown <- root_unknown_files(dat$files$hash, root))) {
    stop(
      sprintf("Can't import metadata for '%s', as files missing:\n%s",
              id, paste(sprintf("  - %s", unknown), collapse = "\n")))
  }
  if (length(unknown <- root_unknown_packets(dat$depends$packet, TRUE, root))) {
    stop(sprintf(
      "Can't import metadata for '%s', as dependencies missing:\n%s",
      id, paste(sprintf("  - %s", unknown), collapse = "\n")))
  }

  writeLines(str, file.path(root$path, ".outpack", "metadata", id))
  location_id <- local_location_id(root)
  time <- Sys.time()
  mark_packet_known(id, location_id, hash, time, root)
  mark_packet_unpacked(id, location_id, time, root)
}


location_path_import_file <- function(path, hash, root) {
  if (!root$config$core$use_file_store) {
    stop("Can't push files into this server, as it does not have a file store")
  }
  root$files$put(path, hash)
}
