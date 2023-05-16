import_zip <- function(zip, root) {
  import_zip_validate(zip::zip_list(zip)$filename)

  tmp <- tempfile()
  zip::unzip(zip, exdir = tmp)
  path <- file.path(tmp, "outpack")

  res <- import_dir_validate(path, root)

  has_archive <- !is.null(root$config$core$path_archive)
  files <- file_store$new(file.path(path, "files"))

  ## At this point we can start importing; start with files as that's
  ## not a big deal:
  if (root$config$core$use_file_store) {
    root$files$import(res$files, files, validate = TRUE)
  }

  ## We don't need to implement this for outpack on the server
  if (!is.null(root$config$core$path_archive)) {
    dst <- file.path(root$path, root$config$core$path_archive,
                     res$unpack$name, res$unpack$id, res$unpack$path)
    if (root$config$core$use_file_store) {
      root$files$get(res$unpack$hash, dst)
    } else {
      ## We do not _want_ to implement this for the server!
      i <- res$unpack$hash %in% res$files
      if (any(i)) {
        files$get(res$unpack$hash[i], dst[i])
      }
      if (any(!i)) {
        src <- vcapply(res$unpack$hash[!i], function(h) {
          find_file_by_hash(root, h) %||% NA_character_
        })
        stopifnot(!any(is.na(src)))
        fs::dir_create(unique(dirname(dst[!i])))
        fs::file_copy(src, dst[!i])
      }
    }
  }

  location_id <- local_location_id(root)
  for (id in names(res$metadata)) {
    write_metadata(res$metadata[[id]], id, Sys.time(), location_id, root)
  }
}


import_zip_validate <- function(file_list) {
  if (!all(vcapply(fs::path_split(file_list), "[[", 1) == "outpack")) {
    stop("Invalid import zip: expected all paths to start with 'outpack/'")
  }
  expected <- c("outpack/contents.json", "outpack/files/", "outpack/metadata/")
  msg <- setdiff(expected, file_list)
  if (length(msg) > 0) {
    stop(sprintf("Invalid import zip: expected %s to exist",
                 paste(squote(msg), collapse = ", ")))
  }
}


import_dir_validate <- function(path, root) {
  contents <- jsonlite::fromJSON(file.path(path, "contents.json"))
  ids <- contents$metadata$id
  json <- vcapply(file.path(path, "metadata", ids), read_string,
                  USE.NAMES = FALSE)

  ## Validate the hashes here first, then validate that we have a
  ## complete tree
  rehash <- function(json, hash) {
    hash_data(json, hash_parse(hash)$algorithm)
  }
  hash <- list_to_character(Map(rehash, json, contents$metadata$hash))
  err <- contents$metadata$hash != hash
  if (any(err)) {
    stop("hash failure")
  }

  ## Next up, validate that we have a complete tree here:
  dat <- lapply(file.path(path, "metadata", ids),
                outpack_metadata_load)
  deps <- unique(unlist(lapply(dat, function(x) x$depends$packet)))
  msg <- setdiff(root_unknown_packets(deps, root), ids)
  if (length(msg) > 0) {
    stop("dependents failure")
  }

  ## Then that we have all the files:
  files <- unique(unlist(lapply(dat, function(x) x$files$hash)))
  msg <- setdiff(root_unknown_files(files, root), contents$files)
  if (length(msg) > 0) {
    stop("files failure")
  }

  metadata <- set_names(json, ids)[order(ids)]

  n <- vapply(dat, function(x) nrow(x$files), numeric(1))
  unpack <- data_frame(
    name = rep(vcapply(dat, "[[", "name"), n),
    id = rep(ids, n),
    path = unlist(lapply(dat, function(x) x$files$path)),
    hash = unlist(lapply(dat, function(x) x$files$hash)))

  list(files = contents$files,
       metadata = metadata,
       unpack = unpack)
}


create_export_zip <- function(plan, root, dest) {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "outpack")
  fs::dir_create(path)

  ## Metadata:
  fs::dir_create(file.path(path, "metadata"))
  fs::file_copy(file.path(root$path, ".outpack", "metadata", plan$packet_id),
                file.path(path, "metadata", plan$packet_id))

  ## We do need to find the files here too, and import them into the
  ## temporary file store:
  store <- file_store$new(file.path(path, "files"))
  if (root$config$core$use_file_store) {
    store$import(plan$files, root$files, validate = FALSE)
  } else {
    for (h in plan$files) {
      store$put(find_file_by_hash(root, h), h)
    }
  }

  ## TODO: do we check that the hashes agree? If so we can avoid this
  ## lookup entirely and just do a match against the metadata.
  idx <- root$index()
  location_id <- local_location_id(root)
  idx_here <- idx$location[idx$location$location == location_id, ]
  hash <- idx_here$hash[match(plan$packet_id, idx_here$packet)]

  contents <- list(metadata = data_frame(id = plan$packet_id, hash = hash),
                   files = plan$files)
  writeLines(to_json(contents, "contents"),
             file.path(path, "contents.json"))

  zip::zip(dest, "outpack", root = tmp)
}
