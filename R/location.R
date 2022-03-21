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
