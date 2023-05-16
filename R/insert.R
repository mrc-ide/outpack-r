outpack_insert_packet <- function(path, json, root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  meta <- outpack_metadata_load(json)
  outpack_log_debug(root, "insert", meta$id, "outpack:::outpack_insert_packet")

  assert_directory(path)

  location_id <- local_location_id(root)
  packet_id <- meta$id

  index <- root$index()
  exists <- any(index$location$packet == packet_id &
                index$location$location == location_id)
  if (exists) {
    stop(sprintf("'%s' has already been added for '%s'",
                 packet_id, local))
  }

  for (i in seq_len(nrow(meta$depends))) {
    validate_packet_has_file(root, meta$depends$packet[[i]],
                             meta$depends$files[[i]]$there)
  }

  ## LOGGING: Report on things like the number of files added to the
  ## archives
  if (root$config$core$use_file_store) {
    file_import_store(root, path, meta$files$path, meta$files$hash)
  }
  if (!is.null(root$config$core$path_archive)) {
    file_import_archive(root, path, meta$files$path, meta$name, packet_id)
  }

  write_metadata(json, packet_id, Sys.time(), location_id, root)

  ## If we were going to add a number in quick succession we could
  ## avoid churn here by not rewriting at every point.
  root$index()
}


write_metadata <- function(json, packet_id, time, location_id, root) {
  hash_algorithm <- root$config$core$hash_algorithm
  hash <- hash_data(json, hash_algorithm)
  path_meta <- file.path(root$path, ".outpack", "metadata", packet_id)
  if (!file.exists(path_meta)) {
    writeLines(json, path_meta)
  }
  mark_packet_known(packet_id, location_id, hash, time, root)
  mark_packet_unpacked(packet_id, location_id, time, root)
}


mark_packet_known <- function(packet_id, location_id, hash, time, root) {
  dest <- file.path(root$path, ".outpack", "location", location_id, packet_id)
  if (!file.exists(dest)) {
    dat <- list(schema_version = scalar(outpack_schema_version()),
                packet = scalar(packet_id),
                time = scalar(time_to_num(time)),
                hash = scalar(hash))
    fs::dir_create(dirname(dest))
    writeLines(to_json(dat, "location"), dest)
  }
}


mark_packet_unpacked <- function(packet_id, location_id, time, root) {
  dest <- file.path(root$path, ".outpack", "unpacked", packet_id)
  if (!file.exists(dest)) {
    dat <- list(schema_version = scalar(outpack_schema_version()),
                packet = scalar(packet_id),
                time = scalar(time_to_num(time)),
                location = scalar(location_id))
    fs::dir_create(dirname(dest))
    writeLines(to_json(dat, "unpacked"), dest)
  }
}
