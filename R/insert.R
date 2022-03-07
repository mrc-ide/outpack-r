outpack_insert_packet <- function(path, json, root = NULL) {
  root <- outpack_root_locate(root)
  meta <- outpack_metadata_load(json)
  assert_directory(path)

  ## TODO(RFC): Is 'local' really the only valid choice here?  It feels
  ## like we could allow for temporary locations and implement
  ## transactions this way.
  location <- "local"

  hash_algorithm <- root$config$core$hash_algorithm

  ## At this point we need to require that 'id' is not known to the
  ## system at least in any remote, but possibly also not in the
  ## entire metadata store?
  id <- meta$id

  ## TODO: For 'insert', rather than 'import', do we want to check for
  ## *any* packet that exists?  For now it's academic as there's no
  ## equivalent to "pull" so this is the only way that things might
  ## appear.
  index <- root$index_update(location)
  if (any(index$location$id == id & index$location$location == location)) {
    stop(sprintf("'%s' has already been added for '%s'", id, location))
  }

  for (i in seq_len(nrow(meta$depends))) {
    validate_dependency(meta$depends$id[[i]], meta$depends$files[[i]]$source,
                        root)
  }

  ## LOGGING: Report on things like the number of files added to the
  ## archives

  ## TODO: add a method to the store for bulk import-and-verify and/or
  ## put the hash arg into put to request validation.
  file_import_store(root, path, meta$files$path, meta$files$hash)
  file_import_archive(root, path, meta$files$path, meta$name, meta$id)

  path_meta <- file.path(root$path, ".outpack", "metadata", id)
  writeLines(json, path_meta)

  ## TODO: once we get more flexible remotes, this will get moved into
  ## its own thing.
  path_meta_loc <- file.path(root$path, ".outpack", "location", location, id)
  meta_loc <- list(schemaVersion = scalar(outpack_schema_version()),
                   id = scalar(id),
                   time = scalar(time_to_num(Sys.time())),
                   hash = scalar(hash_data(json, hash_algorithm)))
  fs::dir_create(dirname(path_meta_loc))
  json <- to_json(meta_loc, "location")
  writeLines(json, path_meta_loc)

  ## If we were going to add a number in quick succession we could
  ## avoid churn here by not rewriting at every point.
  root$index_update(location)
}


validate_dependency <- function(id, path, root) {
  ## TODO: wrap this in tryCatch/withCallingHandlers or similar to get
  ## better error, or make this part of the metadata call (a 'reason'
  ## arg?).  This issue will appear elsewhere too.
  meta <- root$metadata(id)
  err <- setdiff(path, meta$files$path)
  if (length(err) > 0) {
    stop(sprintf("Packet '%s' does not contain path %s",
                 id, paste(squote(err), collapse = ", ")))
  }
}
