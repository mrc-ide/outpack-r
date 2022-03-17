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

  if (name %in% c("local", "orphan")) {
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
##' @export
outpack_location_list <- function(root = NULL) {
  ## TODO: similar to `git remote -v` we might support an extended
  ## mode here (returning a data.frame) or a second function that
  ## returns richer information about the locations.  This function is
  ## going to be called fairly frequently so the cheap version is
  ## important.
  root <- outpack_root_locate(root)
  union("local", names(root$config$location))
}
