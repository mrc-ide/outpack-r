outpack_insert_packet <- function(path, json, root = NULL, verbose = FALSE) {
  root <- outpack_root_locate(root)
  meta <- outpack_metadata_load(json)
  assert_file_exists(path)

  ## TODO: Is 'local' really the only valid choice here?  It feels
  ## like we could allow for temporary locations and implement
  ## transactions this way.
  location <- "local"

  ## TODO: we should get this from the configuration; it should be
  ## consistent with the version used to create the json too, of
  ## course, but that's not totally easy to check.  This is suggesting
  ## that it might be best to just use a single algorithm everywhere.
  ## sha1 is probably enough and is the fastest of the 3 obvious
  ## choices.
  hash_algorithm <- config$core$hash_algorithm

  ## assert directory too, probably...

  ## assert that we're located within root?  That would depend on
  ## configuration as if we were doing bare only it would not matter.

  ## TODO: At this point we need to require that 'id' is not known to
  ## the system.
  id <- meta$id
  index <- root$index_update(location)
  if (any(index$location$id == id & index$location$location == location)) {
    stop(sprintf("'%s' has already been added for '%s'", id, location))
  }

  ## TODO: add a method to the store for bulk import-and-verify and/or
  ## put the hash arg into put to request validation.  It's important that the

  n_files <- nrow(meta$files)
  if (root$config$core$use_file_store) {
    if (verbose) {
      cli::cli_progress_step("Adding {n_files} file{?s} to file store")
    }
    store <- root$files
    for (i in seq_len(n_files)) {
      p <- file.path(path, meta$files$path[[i]])
      h <- store$put(p, hash_algorithm)
      if (h != meta$files$hash[[i]]) {
        stop("Hashes do not match") # TODO: user actionable error
      }
    }
  }

  if (!is.null(root$config$core$path_archive)) {
    if (verbose) {
      cli::cli_progress_step("Adding {n_files} file{?s} to archive")
    }
    dest <- file.path(root$path, root$config$core$path_archive,
                      meta$name, meta$id)
    ## TODO: open question as to if we should filter this down to just
    ## the required files.  We could do a copy of file.path(path,
    ## meta$files$path) into dest, but that does require some care
    ## with path components that have directories.
    fs::dir_copy(path, dest)
  }

  if (verbose) {
    cli::cli_progress_step("Writing out new metadata")
  }
  path_meta <- file.path(root$path, ".outpack", "metadata", id)
  writeLines(json, path_meta)

  path_meta_loc <- file.path(root$path, ".outpack", "location", location, id)
  meta_loc <- list(schemaVersion = scalar(outpack_schema_version()),
                   id = scalar(id),
                   time = scalar(time_to_num()),
                   hash = scalar(hash_data(json, hash_algorithm)))
  fs::dir_create(dirname(path_meta_loc))
  jsonlite::write_json(meta_loc, path_meta_loc)

  if (verbose) {
    cli::cli_progress_done()
  }
  root$index_update(location, verbose = verbose)
}


outpack_pull_packet <- function(id, root = NULL) {
  ## TODO: would we ever want to pass location here directly?  It
  ## could override any selection, and require that we do find the id
  ## at that location.
  root <- outpack_root_locate(root)
  idx <- root$index_update()

  ## These errors might indicate we should refresh
  if (!(id %in% idx$index$id)) {
    stop("id not found")
  }

  ## TODO: check if this exists already, optionally validate?

  found <- idx$location[idx$location$id == id, ]
  if (nrow(found) == 0) {
    stop("id not found on any location")
  }

  ## Here we should consider which we go for? This requires the trust
  ## and preference system
  if (nrow(found) > 1) {
    stop("Implement trust")
  }

  name <- idx$metadata[[id]]$name
  dst <- file.path(root$root, "archive", name, id)
  loc <- outpack_location_get(found$location, root)
  outpack_pull_packet_dest(id, dst, loc)
}


## This will be the general interface and can be used to pull to
## anywhere, regardless of whether it is an existing outpack location.
## This would allow use of outpack data from entirely outpack-free
## workflows.
##
## TODO: needs a better name
outpack_pull_packet_dest <- function(id, dest, loc, ...) {
  fs::dir_create(dest)

  ## When doing the pull, we should work out what we already have and
  ## offer to the remote to not send us these files already.  To do
  ## this we could use the metadata to list what we can find in our
  ## local copies.  That will require a little more work I think (and
  ## we need to validate that these do exist and are correct).  If we
  ## do that then the upstream will pass back a zip file with the new
  ## files only and it is up to the client to copy over existing
  ## files.
  loc$driver$pull_directory(id, dest)
}
