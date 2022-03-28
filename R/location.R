##' Add a new location - a place where other packets might be found
##' and pulled into your local archive.  Currently only file-based
##' locations are supported.
##'
##' @section Warning:
##'
##' The API here will change as we move to support different types of
##'   locations.
##'
##' @title Add a new location
##'
##' @param name The short name of the location to use.  Cannot be in
##'   use, and cannot be one of `local` or `orphan`
##'
##' @param path The path to the directory containing another outpack
##'   archive.
##'
##' @inheritParams outpack_location_list
##'
##' @return Nothing
##' @export
outpack_location_add <- function(name, path, root = NULL) {
  root <- outpack_root_locate(root)
  assert_scalar_character(name)

  if (name %in% location_reserved_name) {
    stop(sprintf("Cannot add a location with reserved name '%s'",
                 name))
  }
  if (name %in% outpack_location_list(root)) {
    stop(sprintf("A location with name '%s' already exists",
                 name))
  }

  ## We won't be necessarily be able to do this _generally_ but here,
  ## let's confirm that we can read from the outpack archive at the
  ## requested path; this will just fail but without providing the
  ## user with anything actionable yet.
  assert_scalar_character(path)
  outpack_root_open(path)

  config <- root$config

  loc <- list(name = name, type = "path", path = path)
  config$location <- c(unname(config$location), list(loc))
  config_write(config, root$path)

  root$config <- config_read(root$path)
  invisible()
}


##' List known locations.
##'
##' @title List known pack locations
##'
##' @param root The outpack root. Will be searched for from the
##'   current directory if not given.
##'
##' @return A character vector of location names. The special name
##'   `local` will always be present.
##'
##' @seealso [outpack::outpack_location_pull_metadata], which can
##'   update your outpack index with metadata from any of the
##'   locations listed here.
##'
##' @export
outpack_location_list <- function(root = NULL) {
  ## TODO: similar to `git remote -v` we might support an extended
  ## mode here (returning a data.frame) or a second function that
  ## returns richer information about the locations.  This function is
  ## going to be called fairly frequently so the cheap version is
  ## important.
  root <- outpack_root_locate(root)
  union(local, names(root$config$location))
}



##' Pull metadata from a location, updating the index.  This should
##' always be relatively quick as it updates only small files that
##' contain information about what can be found in remote packets.
##'
##' @title Pull metadata from a location
##'
##' @param location The name of a location to pull from (see
##'   [outpack::outpack_location_list] for possible values).  If not
##'   given, pulls from all locations.  The "local" and "orphan"
##'   locations are always up to date and pulling metadata from them
##'   does nothing.
##'
##' @inheritParams outpack_location_list
##'
##' @return Nothing
##'
##' @export
outpack_location_pull_metadata <- function(location = NULL, root = NULL) {
  root <- outpack_root_locate(root)
  if (is.null(location)) {
    location <- outpack_location_list(root)
  }
  assert_character(location)

  for (name in setdiff(location, location_reserved_name)) {
    location_pull_metadata(name, root)
  }
}


##' Pull a packet (all files) from a location into this archive. This
##' will make files available for use as dependencies (e.g., with
##' [outpack::outpack_packet_use_dependency])
##'
##' The behaviour of this function will vary depending on whether or
##' not the destination outpack repository (i.e., `root`) uses a file
##' store or not.  If it does, then we simply import the unknown files
##' into the store, and this will always be fairly efficient.  If no
##' file store is used then for the time being we pull all files from
##' the upstream location, even if this means copying a file we
##' already know about elsewhere in the outpack archive.  We will
##' improve this in a future version.
##'
##' @title Pull a single packet from a location
##'
##' @param id The id of the packet(s) to pull
##'
##' @param location The name of the location to pull from.  Later we
##'   will relax this (see mrc-3030)
##'
##' @param recursive Logical, indicating if we should recursively pull
##'   all packets that are referenced by the packets specified in
##'   `id`.  This might copy a lot of data!
##'
##' @inheritParams outpack_location_list
##'
##' @return Invisibly, the ids of packets that were pulled
##' @export
outpack_location_pull_packet <- function(id, location, recursive = FALSE,
                                         root = NULL) {
  root <- outpack_root_locate(root)
  assert_character(id)
  index <- root$index()

  ## We are restricting this to a single location, but if all
  ## locations are trustable, then we might want instead to look over
  ## all known locations as the files are just files (mrc-3030)
  if (!any(index$location$packet == id & index$location$location == location)) {
    stop(sprintf(
      "packet '%s' not known at location '%s' (consider pulling metadata)",
      id, location))
  }
  driver <- location_driver(location, root)

  if (recursive) {
    id <- find_all_dependencies(id, index$metadata)
  }

  ## Later, it might be better if we did not skip over unpacked
  ## packets, but instead validate and/or repair them (see mrc-3052)
  id <- setdiff(id, index$unpacked$packet)

  ## At this point we should really be providing logging about how
  ## many packets, files, etc are being copied.  I've done this as a
  ## single loop, but there's also no real reason why we might not
  ## present this as a single update operation for pulling all files
  ## across all packets.  This is the simplest implementation for now
  ## though.
  ##
  ## Making this nicer might be easiest to do by updating
  ## outpack_location_pull_packet to accept a vector of ids and having
  ## it resolve all missing files at once, which would complicate that
  ## a little?
  ##
  ## However, the exposed interface to the user (aside from progress
  ## reporting) will not change.
  for (i in id) {
    location_pull_files_store(root, driver, i)
    location_pull_files_archive(root, driver, i)
    mark_packet_unpacked(i, location, root)
  }

  invisible(id)
}


location_driver <- function(location, root) {
  dat <- root$config$location[[location]]
  if (is.null(dat)) {
    stop(sprintf("Unknown location '%s'", location))
  }
  ## Once we support multiple location types, we'll need to consider
  ## this more carefully; leaving an assertion in to make it more
  ## obvious where change is needed.
  stopifnot(dat$type == "path")
  outpack_location_path$new(dat$path)
}


location_pull_metadata <- function(location_name, root) {
  index <- root$index()
  driver <- location_driver(location_name, root)

  known_there <- driver$list()

  ## Things we've never heard of from any location:
  new_id_metadata <- setdiff(known_there$packet, names(index$metadata))
  if (length(new_id_metadata) > 0) {
    metadata <- driver$metadata(new_id_metadata)
    path_metadata <- file.path(root$path, ".outpack", "metadata")
    fs::dir_create(path_metadata)
    filename <- file.path(path_metadata, new_id_metadata)
    for (i in seq_along(metadata)) {
      writeLines(metadata[[i]], filename[[i]])
    }
  }

  known_here <- index$location$packet[index$location$location == location_name]
  new_loc <- known_there[!(known_there$packet %in% known_here), ]

  for (i in seq_len(nrow(new_loc))) {
    mark_packet_known(new_loc$packet[[i]], location_name, new_loc$hash[[i]],
                      new_loc$time[[i]], root)
  }

  root$index()
}


## This will work across any number of packets at once with a small
## amount of change.
location_pull_files_store <- function(root, driver, id) {
  if (root$config$core$use_file_store) {
    meta <- root$metadata(id)
    files_exist <- root$files$exists(meta$files$hash)
    for (h in meta$files$hash[!files_exist]) {
      ## TODO: Should we support bulk download? This might be more
      ## efficient for some drivers (e.g., async http or streaming a
      ## single zip)
      root$files$put(driver$fetch_file(h), h)
    }
  }
}

location_pull_files_archive <- function(root, driver, id) {
  if (!is.null(root$config$core$path_archive)) {
    meta <- root$metadata(id)
    dest <- file.path(root$path, root$config$core$path_archive, meta$name, id,
                      meta$files$path)
    if (root$config$core$use_file_store) {
      for (i in seq_len(nrow(meta$files))) {
        root$files$get(meta$files$hash[[i]], dest[[i]])
      }
    } else {
      ## TODO: some special care needed here if we want to avoid
      ## downloading the same file twice from _this_ packet as we
      ## won't be able to use find_file_by_hash function to resolve
      ## that.
      fs::dir_create(dirname(dest))
      for (i in seq_len(nrow(meta$files))) {
        hash <- meta$files$hash[[i]]
        src <- find_file_by_hash(root, hash) %||% driver$fetch_file(hash)
        fs::file_copy(src, dest[[i]])
      }
    }
  }
}
