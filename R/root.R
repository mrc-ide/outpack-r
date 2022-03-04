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

    ## TODO: remove this method until next PR?
    location_last_update = function(location = NULL) {
      index <- self$index_update()
      if (is.null(location)) {
        date <- index$location$date
      } else {
        date <- index$location$date[index$location$location %in% location]
      }
      max_time(date)
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
      if (is.null(locations)) {
        locations <- self$location_list()
      }
      private$index_data <- index_read(locations, self$path, private$index_data,
                                       refresh)
      invisible(private$index_data)
    }
  ))


outpack_root_locate <- function(path) {
  if (inherits(path, "outpack_root")) {
    return(path)
  }
  root_found <- find_file_descend(".outpack", path %||% getwd())
  if (is.null(root_found)) {
    stop(sprintf("Did not find existing outpack root from directory '%s'",
                 path %||% "."))
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



## TODO: confusingly this is really index_update, not read

## The index consists of a few bits:
## $index - data.frame of name/id pairs (could also save this as
##          name split by id)
## $location - data.frame of id, location and date
## $metadata - named list of full metadata
index_read <- function(locations, root, prev, refresh) {
  path_index <- file.path(root, ".outpack", "index", "outpack.rds")

  if (refresh) {
    data <- list()
  } else if (is.null(prev)) {
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

  ## TODO(RFC): There's a good reason here to wonder if this _should_
  ## be configurable.  I'll keep it here within the configuration even
  ## though it can't be changed really.
  hash_algorithm <- "sha256"

  cfg <- list(
    schemaVersion = scalar(outpack_schema_version()),
    core = list(
      path_archive = scalar(path_archive),
      use_file_store = scalar(use_file_store),
      hash_algorithm = scalar(hash_algorithm)))
  to_json(cfg)
}
