outpack_location_list <- function(root = NULL) {
  root <- outpack_root_locate(root)
  root$location_list()
}


outpack_location <- function(name, driver, args) {
  if (driver == "path") {
    driver <- outpack_location_path
  } else {
    stop("Not yet implemented")
  }
  structure(
    list(name = name,
         driver = do.call(outpack_location_path$new, args)),
    class = "outpack_location")
}


## This part will need changing as soon as we start on multilanguage
## setups, because we really need this to work for everything.
##
## We might treat 'path' specially, but for anything else we will need
## drivers that come in the form of packages that will have different
## names and conventions (but that we might want to have some control
## to enforce shared args).
##
## We also need to make sure that we can update the configuration
## somewhat atomically...
##
## TODO: do we want to split the location directory a bit?
## .outpack/location/<name>/
##   config.rds
##   files/
## wait until we sort out the python interface.
outpack_location_add <- function(name, driver, ..., args = list(...),
                                 root = NULL, update_metadata = TRUE,
                                 verbose = TRUE) {
  root <- outpack_root_locate(root)
  check_location_name(name, root$location_list())

  if (verbose) {
    cli::cli_progress_step("Validating location")
  }

  ## TODO: this is currently saved as rds but we will want to save as
  ## json to allow other interfaces to pull from remotes.
  assert_scalar_character(driver)
  data <- list(driver = driver, args = args)
  loc <- outpack_location(name, driver, args)
  loc$driver$list()

  path_cfg <- file.path(root$root, ".outpack", "location", name, "config.rds")
  fs::dir_create(dirname(path_cfg))
  saveRDS(data, path_cfg)

  cli::cli_progress_done()

  if (update_metadata) {
    outpack_location_update_metadata(name, root = root, verbose = verbose)
  }
}


outpack_location_remove <- function(location, root = NULL, verbose = TRUE) {
  root <- outpack_root_locate(root)
  location <- outpack_location_get(location, root)
  index <- root$index_update()

  ids_here <- index$location$id[index$location$location == location$name]
  ids_elsewhere <- index$location$id[index$location$location != location$name]
  ids_orphan <- setdiff(ids_here, ids_elsewhere)
  if (length(ids_orphan) > 0) {
    path_orphan <- file.path(root$root, ".outpack", "location", "orphan")
    path_here <- file.path(root$root, ".outpack", "location", location$name)
    if (verbose) {
      cli::cli_alert_info("Orphaning {length(ids_orphan)} packet{?s}")
    }
    fs::dir_create(path_orphan)
    fs::file_move(file.path(path_here, ids_orphan), path_orphan)
  } else {
    if (verbose) {
      cli::cli_alert_info("No orphans created")
    }
  }
  fs::dir_delete(path_here)
  root$index_update(verbose = verbose)
}


outpack_location_rename <- function(location, new, root = NULL,
                                    verbose = TRUE) {
  root <- outpack_root_locate(root)
  location <- outpack_location_get(location, root)
  check_location_name(new, root$location_list())

  path_old <- file.path(root$root, ".outpack", "location", location$name)
  path_new <- file.path(root$root, ".outpack", "location", new)

  fs::file_move(path_old, path_new)
  root$index_update(verbose = verbose)
}


## TODO: cope with being passed a location object
## TODO: cope with credential caching
## TODO: cope with environment variable resolution?
## TODO: centralise this path logic elsewhere
outpack_location_get <- function(location, root = NULL) {
  root <- outpack_root_locate(root)
  assert_scalar_character(location)
  if (location %in% c("local", "orphan")) {
    stop(sprintf("Invalid use of reserved location '%s'", location))
  }
  path_config <- file.path(root$root, ".outpack", "location", location,
                           "config.rds")
  if (!file.exists(path_config)) {
    stop(sprintf("Can't use location '%s'", location))
  }

  data <- readRDS(path_config)
  outpack_location(location, data$driver, data$args)
}


outpack_location_update_metadata <- function(location, root = NULL,
                                             verbose = TRUE) {
  ## 1. fetch root
  root <- outpack_root_locate(root)

  ## 2. fetch location object
  location <- outpack_location_get(location, root)

  ## 3. look up current time, if known, for this location in the index
  last_update <- root$location_last_update(location$name)

  ## 4. request all new metadata newer than this time
  if (verbose) {
    cli::cli_progress_step("Checking for new metadata")
  }
  new <- location$driver$list(last_update)
  if (length(new$id) == 0) {
    cli::cli_progress_done()
    cli::cli_alert_info("No new metadata")
    return()
  }

  index <- root$index_update()
  new_id <- setdiff(new$id, index$index$id)

  ## 5. write out list of strings within metadata/<location>
  ##
  ## So, here, if we've already seen an index anywhere we will avoid
  ## refetching it again but we should consider checking the hashes do
  ## actually agree.  This needs some work so that we can orient
  ## things so that the user could actually do something about it.
  if (length(new_id) > 0) {
    if (verbose) {
      cli::cli_progress_step(
        "Fetching metadata for {length(new_id)} packet{?s}")
    }

    ## TODO: do we need to validate the hash here?
    metadata <- location$driver$metadata(new_id)

    if (verbose) {
      cli::cli_progress_step("Writing metadata")
    }
    path_metadata <- file.path(root$root, ".outpack", "metadata")
    fs::dir_create(path_metadata)
    filename <- file.path(path_metadata, new_id)
    for (i in seq_along(new_id)) {
      writeLines(metadata[[i]], filename[[i]])
    }
  }

  ## Bit of a nuisance here:
  if (verbose) {
    cli::cli_progress_step(
      "Writing location information for {length(new$id)} packet{?s}")
  }
  path_location <- file.path(root$root, ".outpack", "location", location$name)
  fs::dir_create(path_location)
  for (i in seq_along(new$id)) {
    d <- list(id = scalar(new$id[[i]]),
              date = scalar(new$time),
              hash = scalar(new$hash[[i]]))
    filename <- file.path(path_location, new$id[[i]])
    jsonlite::write_json(d, filename)
  }

  cli::cli_progress_done()

  ## Here we should drop verbosity down by one, I think
  root$index_update(verbose = verbose)
}


check_location_name <- function(location, existing) {
  assert_scalar_character(location)
  if (location %in% c("local", "orphan")) {
    stop(sprintf("'%s' is a reserved location name", location))
  }
  if (location %in% existing) {
    stop(sprintf("'%s' is already a location for this outpack", location))
  }
}
