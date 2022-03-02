##' Create a new outpack root
##'
##' @title Create outpack root
##'
##' @param root Path to use
##'
##' @return An `outpack_root` object
outpack_init <- function(root, verbose = TRUE) {
  path_outpack <- file.path(root, ".outpack")
  if (file.exists(path_outpack)) {
    cli::cli_alert_info("outpack already initialised at '{root}'")
    return(invisible(outpack_root$new(root)))
  }

  fs::dir_create(path_outpack)

  ## Things that we might do here (but don't yet):
  ##
  ## * Write out some version information so that we can gracefully
  ##   handle any migration

  writeLines(outpack_config_default(), file.path(path_outpack, "config.json"))

  fs::dir_create(file.path(path_outpack, "metadata"))
  fs::dir_create(file.path(path_outpack, "location"))

  ## We need to configure our local location at this point, or is that
  ## always implied?

  ## TODO: edit gitignore to add .outpack and archive to it

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
      ## Good chance we don't want this activated all the time,
      ## really, just on demand.
      self$files <- file_store$new(file.path(path, ".outpack", "files"))
      lockBinding("path", self)
      lockBinding("config", self)
      lockBinding("files", self)
    },

    ## TODO: this needs an extended form with notions of trust.
    location_list = function() {
      union("local", dir(file.path(self$path, ".outpack", "location")))
    },

    location_last_update = function(location = NULL) {
      index <- self$index_update()
      if (is.null(location)) {
        date <- index$location$date
      } else {
        date <- index$location$date[index$location$location %in% location]
      }
      max_time(date)
    },

    index_update = function(locations = NULL, refresh = FALSE,
                            verbose = FALSE) {
      if (is.null(locations)) {
        locations <- self$location_list()
      }
      private$index_data <- index_read(locations, self$path, private$index_data,
                                       refresh, verbose)
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
  ## TODO: If we're more relaxed here about format, then this is
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
index_read <- function(locations, root, prev, refresh, verbose) {
  ## TODO: option to validate hashes, or do we do that on import?

  ## TODO: we should have a validate function somewhere that
  ## checks all in the directory are of this form exactly
  path_index <- file.path(root, "index", "outpack.rds")

  if (refresh) {
    data <- list()
  } else if (is.null(prev)) {
    data <- if (file.exists(path_index)) readRDS(path_index) else list()
  } else {
    data <- prev
  }

  if (verbose) {
    cli::cli_progress_step("Indexing data for {length(locations)} location{?s}")
  }
  data$location <- read_locations(locations, root, data$location)

  ## Work out what we've not yet seen and read that:
  id_new <- setdiff(data$location$id, data$index$id)

  if (length(id_new) > 0) {
    if (verbose) {
      cli::cli_progress_step(
        "Indexing metadata for {length(id_new)} packet{?s}")
    }
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
    if (verbose) {
      cli::cli_progress_step("Writing index")
    }
    saveRDS(data, path_index)
  }

  data
}
