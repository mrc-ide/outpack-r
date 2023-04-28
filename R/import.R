import_zip <- function(zip, root) {
  import_zip_validate(zip)

  tmp <- tempfile()
  zip::unzip(zip, exdir = tmp)
  path <- file.path(tmp, "outpack")

  res <- import_validate(path, root)

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
        browser()
        foo <- vcapply(res$unpack$hash[!i], find_file_by_hash, root = root)
        fs::dir_create(unique(dirname(dst[!i])))
        fs::file_copy(foo, dst[!i])
      }
    }
  }

  location_id <- local_location_id(root)
  for (id in names(res$metadata)) {
    write_metadata(res$metadata[[id]], id, Sys.time(), location_id, root)
  }
}


sitrep <- function(ids, driver, root) {
  ## The first part is to get the full set of ids within a chain - we
  ## have support for this in the query index but I am not certain
  ## that's the best way to achive this.
  ids <- recursive_dependencies(ids, root)

  ## Which of these does the server not know about:
  ids_msg <- driver$unknown_packets(ids)

  if (length(ids_msg) == 0) {
    files_msg <- character(0)
  } else {
    metadata <- root$index()$metadata
    ## All files across all missing ids:
    files <- unique(unlist(
      lapply(ids_msg, function(i) metadata[[i]]$files$hash)))

    ## Which of these does the server not know about:
    files_msg <- driver$unknown_files(files)
  }

  list(ids = ids_msg, files = files_msg)
}


create_zip <- function(ids, driver, root, dest) {
  dat <- sitrep(ids, driver, root)
  export_zip(root, dat$ids, dat$files, dest)
  driver$import(dest)
}


export_zip <- function(root, ids, files, dest) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))

  path <- file.path(tmp, "outpack")
  fs::dir_create(path)

  ## Metadata:
  fs::dir_create(file.path(path, "metadata"))
  fs::file_copy(file.path(root$path, ".outpack", "metadata", ids),
                file.path(path, "metadata", ids))

  ## We do need to find the files here too, and import them into the
  ## temporary file store:
  store <- file_store$new(file.path(path, "files"))
  if (is.null(root$files)) {
    for (h in files) {
      store$put(find_file_by_hash(root, h), h)
    }
  } else {
    store$import(files_msg, root$files, validate = FALSE)
  }

  ## TODO: do we check that the hashes agree? If so we can avoid this
  ## lookup entirely and just do a match against the metadata.
  idx <- root$index()
  idx_here <-
    idx$location[idx$location$location == lookup_location_id("local", root), ]
  hash <- idx_here$hash[match(ids, idx_here$packet)]

  contents <- list(
    metadata = data_frame(id = ids, hash = hash),
    files = files)
  writeLines(to_json(contents, "contents"),
             file.path(path, "contents.json"))

  zip::zip(dest, "outpack", root = tmp)
  dest
}


import_zip_validate <- function(file) {
  files <- zip::zip_list(file)$filename
  if (!all(vcapply(fs::path_split(files), "[[", 1) == "outpack")) {
    stop("Invalid import zip: expected all paths to start with 'outpack/'")
  }
  expected <- c("outpack/contents.json", "outpack/files/", "outpack/metadata/")
  for (f in expected) {
    if (!(f %in% files)) {
      stop(sprintf("Invalid import zip: expected '%s' to exist", f))
    }
  }
}


import_validate <- function(path, root) {
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
