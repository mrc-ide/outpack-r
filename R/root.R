outpack_init <- function(root) {
  ## TODO: might be safest to only allow relative paths here?
  path_outpack <- file.path(root, ".outpack")
  if (file.exists(path_outpack)) {
    message(sprintf("outpack already initialised at '%s'", root))
    outpack_root$new(root)
  }

  fs::dir_create(path_outpack)

  ## Things that we might do here (but don't yet):
  ##
  ## * Write out some version information so that we can gracefully
  ##   handle any migration
  ##
  ## * Write out some configuration file (once we have things worth tweaking)

  fs::dir_create(file.path(path_outpack, "metadata"))
  fs::dir_create(file.path(path_outpack, "location"))

  ## We need to configure our local location at this point, or is that
  ## always implied?

  ## TODO: edit gitignore to add .outpack and archive to it

  outpack_root$new(root)
}


outpack_root <- R6::R6Class(
  "outpack_root",
  cloneable = FALSE,

  private = list(
    index_data = NULL
  ),

  public = list(
    root = NULL,

    initialize = function(root) {
      assert_file_exists(root)
      assert_file_exists(file.path(root, ".outpack"))
      self$root <- root
      lockBinding("root", self)
    },

    ## TODO: this needs an extended form with notions of trust.
    location_list = function() {
      union("local", dir(file.path(self$root, ".outpack", "location")))
    },

    last_update = function(location = NULL) {
      index <- self$index_update()
      if (is.null(location)) {
        date <- index$location$date
      } else {
        date <- index$location$date[index$location$location %in% location]
      }
      max_time(date)
    },

    index_update = function(refresh = FALSE, verbose = FALSE) {
      private$index_data <- read_index(
        self$location_list(), self$root, self$index_data, refresh, verbose)
      invisible(private$index_data)
    }
  ))


outpack_root_locate <- function(root) {
  if (inherits(root, "outpack_root")) {
    return(root)
  }
  root_found <- find_file_descend(".outpack", root %||% getwd())
  if (is.null(root_found)) {
    stop(sprintf("Did not find existing outpack root from directory '%s'",
                 root %||% "."))
  }
  outpack_root$new(root_found)
}


outpack_root_open <- function(root) {
  if (inherits(root, "outpack_root")) {
    return(root)
  }
  assert_scalar_character(root)
  assert_file_exists(root)
  if (!file.exists(file.path(root, ".outpack"))) {
    stop(sprintf("'%s' does not look like an outpack root", root))
  }
  outpack_root$new(root)
}


read_location <- function(location, root, prev) {
  re <- "^([0-9]{8}-[0-9]{6}-[[:xdigit:]]{8})\\.json$"
  path <- file.path(root, ".outpack", "location", location)
  files <- dir(path, re)
  is_new <- !(sub(re, "\\1", files) %in% prev$id[prev$location == location])
  if (!any(is_new)) {
    return(NULL)
  }
  dat <- lapply(file.path(path, files[is_new]), jsonlite::read_json)
  cols <- c("id", "date", "hash")
  ret <- lapply(cols, function(v) vcapply(dat, "[[", v))
  names(ret) <- cols
  ret$date <- as.POSIXct(ret$date, "UTC")
  ret$location <- location

  as.data.frame(ret, stringsAsFactors = FALSE)
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
read_index <- function(locations, root, prev, refresh, verbose) {
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
  location <- read_locations(locations, root, data$location)
  data$location <- location

  ## Work out what we've not yet seen and read that:
  id_new <- setdiff(location$id, data$index$id)

  if (length(id_new) > 0) {
    if (verbose) {
      cli::cli_progress_step(
        "Indexing metadata for {length(id_new)} packet{?s}")
    }
    files <- file.path(root, ".outpack", "metadata", paste0(id_new, ".json"))
    metadata_new <- lapply(files, outpack_metadata_read_index)
    index_new <- data.frame(
      data.frame(name = vcapply(metadata_new, "[[", "name"),
                 id = vcapply(metadata_new, "[[", "id")))
    data$index <- rbind(data$index, index_new)
    data$metadata <- rbind(data$index, metadata_new)
    fs::dir_create(dirname(path_index))
    if (verbose) {
      cli::cli_progress_step("Writing index")
    }
    saveRDS(data, path_index)
  }

  data
}
