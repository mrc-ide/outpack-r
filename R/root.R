##' Create a new outpack root.
##'
##' @title Create outpack root
##'
##' @param root Path to use.  This path may exist, but it is an error
##'   to call this on a path that has already been initialised.
##'
##' @param path_archive Path to the archive directory, used to store
##'   human-readable copies of packets.  If `NULL`, no such copy is
##'   made, and `file_store` must be `TRUE`
##'
##' @param use_file_store Logical, indicating if we should use a
##'   content-addressable file-store as the source of truth for
##'   packets.  If `archive` is non-`NULL`, the file-store will be
##'   used as the source of truth and the duplicated files in archive
##'   exist only for convenience.
##'
##' @return Invisibly, an `outpack_root` object; these will be
outpack_init <- function(root, path_archive = "archive",
                         use_file_store = FALSE) {
  ## Logging: print information about what we're doing here.
  path_outpack <- file.path(root, ".outpack")
  if (file.exists(path_outpack)) {
    stop(sprintf("outpack already initialised at '%s'", path_outpack))
  }

  cfg <- outpack_root_config_new(path_archive, use_file_store)

  fs::dir_create(path_outpack)
  fs::dir_create(file.path(path_outpack, "metadata"))
  fs::dir_create(file.path(path_outpack, "location"))
  writeLines(cfg, file.path(path_outpack, "config.json"))

  invisible(outpack_root$new(root))
}


## TODO: I am torn here on design - we could make most of the things
## that use the root be methods, but that risks a god class.  Getting
## access to the index does require mutability so that must be a
## method, but it's possible that moving to free functions everywhere
## would be best.
outpack_root <- R6::R6Class(
  "outpack_root",
  cloneable = FALSE,

  private = list(
    index_data = NULL
  ),

  public = list(
    path = NULL,
    config = NULL,
    files = NULL,

    initialize = function(path) {
      assert_file_exists(path)
      assert_file_exists(file.path(path, ".outpack"))
      self$path <- path
      self$config <- jsonlite::read_json(
        file.path(path, ".outpack/config.json"))
      if (self$config$core$use_file_store) {
        self$files <- file_store$new(file.path(path, ".outpack", "files"))
      }
      lockBinding("path", self)
      lockBinding("config", self)
      lockBinding("files", self)
    },

    location_list = function() {
      union("local", dir(file.path(self$path, ".outpack", "location")))
    },

    metadata = function(id) {
      ## TODO: this contains more logic than ideal but attempts to
      ## avoid updating the index if needed.  The other thing to do
      ## would _always_ be to update the index but that feels wasteful
      ## really.
      ##
      ## We could probably be much more efficient if we cached all
      ## roots within a session, though doing that safely would
      ## practically mean putting a key file in each root so that we
      ## can detect directory moves.
      meta <- private$index_data$metadata[[id]] %||%
        self$index_update()$metadata[[id]]
      if (is.null(meta)) {
        stop(sprintf("id '%s' not found in index", id))
      }
      meta
    },

    index_update = function(locations = NULL, refresh = FALSE) {
      prev <- if (refresh) list() else private$index_data
      private$index_data <- index_update(
        locations %||% self$location_list(), self$path, prev)
      invisible(private$index_data)
    }
  ))


outpack_root_locate <- function(path) {
  if (inherits(path, "outpack_root")) {
    return(path)
  }
  path <- path %||% "."
  assert_scalar_character(path)
  assert_directory(path)
  root_found <- find_file_descend(".outpack", path)
  if (is.null(root_found)) {
    stop(sprintf("Did not find existing outpack root from directory '%s'",
                 path))
  }
  outpack_root$new(root_found)
}


outpack_root_open <- function(path) {
  if (inherits(path, "outpack_root")) {
    return(path)
  }
  assert_scalar_character(path)
  assert_file_exists(path)
  if (!file.exists(file.path(path, ".outpack"))) {
    stop(sprintf("'%s' does not look like an outpack root", path))
  }
  outpack_root$new(path)
}


read_location <- function(location, root, prev) {
  ## TODO: If we're more relaxed here about format, then this will
  ## need changing.  This regex will end up moving somewhere central
  ## in the package in that case.
  re <- "^([0-9]{8}-[0-9]{6}-[[:xdigit:]]{8})$"
  path <- file.path(root, ".outpack", "location", location)
  ids <- dir(path, re)
  is_new <- !(ids %in% prev$id[prev$location == location])
  if (!any(is_new)) {
    return(NULL)
  }

  dat <- lapply(file.path(path, ids[is_new]), jsonlite::read_json)
  data_frame(id = vcapply(dat, "[[", "id"),
             time = num_to_time(vnapply(dat, "[[", "time")),
             hash = vcapply(dat, "[[", "hash"),
             location = location)
}


read_locations <- function(locations, root, prev) {
  if (NROW(prev) > 0) {
    prev <- prev[prev$location %in% locations, ]
  }
  new <- do.call(rbind, lapply(locations, read_location, root, prev))
  ret <- rbind(prev, new)
  if (NROW(ret) > 0 && NROW(prev) > 0) {
    ret <- ret[order(ret$location), ]
    rownames(ret) <- NULL
  }
  ret
}


## The index consists of a few bits:
## $index - data.frame of name/id pairs (could also save this as
##          name split by id)
## $location - data.frame of id, location and date
## $metadata - named list of full metadata
index_update <- function(locations, root, prev) {
  path_index <- file.path(root, ".outpack", "index", "outpack.rds")

  if (is.null(prev)) {
    data <- if (file.exists(path_index)) readRDS(path_index) else list()
  } else {
    data <- prev
  }

  ## TODO: Add some logging through here.

  data$location <- read_locations(locations, root, data$location)

  ## Work out what we've not yet seen and read that:
  id_new <- setdiff(data$location$id, data$index$id)

  if (length(id_new) > 0) {
    files <- file.path(root, ".outpack", "metadata", id_new)
    metadata_new <- lapply(files, outpack_metadata_index_read)
    names(metadata_new) <- id_new
    index_new <- data.frame(
      data.frame(name = vcapply(metadata_new, "[[", "name"),
                 id = vcapply(metadata_new, "[[", "id")))
    data$index <- rbind(data$index, index_new)
    rownames(data$index) <- NULL
    data$metadata <- c(data$metadata, metadata_new)
    fs::dir_create(dirname(path_index))
    saveRDS(data, path_index)
  }

  data
}


outpack_root_config_new <- function(path_archive, use_file_store) {
  if (!is.null(path_archive)) {
    assert_scalar_character(path_archive)
  }
  assert_scalar_logical(use_file_store)
  if (is.null(path_archive) && !use_file_store) {
    stop("if 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  }

  ## TODO: There's a good reason here to wonder if this _should_ be
  ## configurable.  I'll keep it here within the configuration even
  ## though it can't be changed really.
  hash_algorithm <- "sha256"

  cfg <- list(
    schemaVersion = scalar(outpack_schema_version()),
    core = list(
      path_archive = scalar(path_archive),
      use_file_store = scalar(use_file_store),
      hash_algorithm = scalar(hash_algorithm)))
  to_json(cfg, "config")
}


## Not just for the file store, but this is how we can interact with
## the files safely:
file_export <- function(root, id, path, dest) {
  ## This validation *always* occurs; does the packet even claim to
  ## have this path?
  validate_packet_has_file(root, id, path)
  ## TODO: log file copy information, including hashes.  Because copy
  ## can be slow for large files, we might want to do this file by
  ## file?

  ## TODO: The copy should ideally all succeed or all fail wherever
  ## possible

  ## TODO: check that no dependency destination exists, or offer solution
  ## to overwrite (requires argument here, flowing back to the interface)

  ## TODO: Additional work required to support directory based
  ## dependencies

  fs::dir_create(dirname(dest))

  meta <- root$metadata(id)
  hash <- meta$files$hash[match(path, meta$files$path)]

  if (root$config$core$use_file_store) {
    for (i in seq_along(dest)) {
      root$files$get(hash[[i]], dest[[i]])
    }
  } else {
    src <- file.path(root$path, root$config$core$path_archive,
                     meta$name, meta$id, path)
    assert_file_exists(src)
    ## TODO: Ideally we would have an argument/option support a faster
    ## possibility here if requested (e.g., no validation validate just
    ## size, validate hash); this only applies to this non-file-store
    ## using branch, so typically would affect users running "draft"
    ## type analyses
    for (i in seq_along(dest)) {
      hash_validate(src[[i]], hash[[i]])
    }
    fs::file_copy(src, dest)
  }
}


file_import_store <- function(root, path, file_path, file_hash) {
  if (root$config$core$use_file_store) {
    for (i in seq_along(file_path)) {
      root$files$put(file.path(path, file_path[[i]]), file_hash[[i]])
    }
  }
}


file_import_archive <- function(root, path, file_path, name, id) {
  if (!is.null(root$config$core$path_archive)) {
    dest <- file.path(root$path, root$config$core$path_archive, name, id)

    ## TODO: These should not ever happen, so just asserting here.  If
    ## it does happen it requires that the user has provided an id,
    ## and also copied files around?  Not sure how we'd recover here
    ## either.
    stopifnot(path != dest,
              !file.exists(dest))

    ## TODO: open question as to if we should filter this down to just
    ## the required files (as we do here); this means that if the user
    ## has provided "files" to the metadata function we'd be leaving
    ## some files behind.  This does match the behaviour of the file
    ## store version, but not of orderly.
    file_path_dest <- file.path(dest, file_path)
    fs::dir_create(dirname(file_path_dest))
    fs::file_copy(file.path(path, file_path), file_path_dest)
  }
}


## This might move elsewhere
validate_packet_has_file <- function(root, id, path) {
  ## TODO: wrap this in tryCatch/withCallingHandlers or similar to get
  ## better error, or make this part of the metadata call (a 'reason'
  ## arg?).  This issue will appear elsewhere too.
  meta <- root$metadata(id)
  err <- setdiff(path, meta$files$path)
  if (length(err) > 0) {
    ## TODO: this might also want wrapping so that we report back
    ## better errors.  One possibility here is that we should report
    ## "near misses" (Did you mean: X), though that will be best to
    ## think about fairly broadly as it will likely affect other parts
    ## of the packge.
    stop(sprintf("Packet '%s' does not contain path %s",
                 id, paste(squote(err), collapse = ", ")))
  }
}
