##' Start a packet build (`outpack_packet_start`), end one
##' (`outpack_packet_cancel`, `outpack_packet_end`) and interact with
##' the current packet (`outpack_packet_use_dependency`,
##' `outpack_packet_run`)
##'
##' @title Start, interact with, and end a packet build
##'
##' @param path Path to the build / output directory.
##'
##' @param name The name of the packet
##'
##' @param parameters Optionally, a named list of parameters.  The
##'   names must be unique, and the values must all be non-NA scalar
##'   atomics (logical, integer, numeric, character)
##'
##' @param root The outpack root. Will be searched for from the
##'   current directory if not given.
##'
##' @return Invisibly, a copy of the packet data
##' @rdname outpack_packet
##' @export
outpack_packet_start <- function(path, name, parameters = NULL,
                                 root = NULL) {
  root <- outpack_root_locate(root)
  if (!is.null(current$packet)) {
    ## * Could make this root specific?
    ## * Could make this path specific?
    ## * Could use a stack?
    ## * Could automatically exit?
    ## * Could be fine if name/id match
    stop("Already a current packet - call outpack_packet_cancel()")
  }

  assert_scalar_character(name)
  assert_directory(path)
  validate_parameters(parameters)

  ## TODO: we could accept 'id' as an argument here, but we'd need to
  ## validate that it matches our format (depending on what we chose
  ## to accept) and we should check that it is not present in the
  ## index yet.
  id <- outpack_id()

  time <- list(start = Sys.time())

  ## LOGGING: name / id / path, start time, parameters
  ##
  ## We log these all in orderly and that's super useful

  ## TODO: optionally scan directory for files so that we have pre-run hashes

  ## TODO: optionally copy the directory into some scratch location,
  ## as we do for the test runner

  current$packet <- list(
    name = name,
    id = id,
    path = path,
    parameters = parameters,
    time = time,
    root = root)

  invisible(current$packet)
}


##' @export
##' @rdname outpack_packet
outpack_packet_cancel <- function() {
  p <- outpack_packet_current()
  outpack_packet_clear()
}


##' @export
##' @rdname outpack_packet
outpack_packet_current <- function() {
  if (is.null(current$packet)) {
    stop("No current packet")
  }
  current$packet
}

##' @export
##' @rdname outpack_packet
outpack_packet_end <- function() {
  p <- outpack_packet_current()
  p$time$end <- Sys.time()
  json <- outpack_metadata_create(p$path, p$name, p$id, p$time,
                                  depends = p$depends,
                                  parameters = p$parameters)
  outpack_insert_packet(p$path, json, p$root)
  outpack_packet_clear()
}


##' @export
##' @rdname outpack_packet
##'
##' @param script Path to the script within the packet directory (a
##'   relative path).  This function can be safely called multiple
##'   times within a single packet run (or zero times!) as needed.
outpack_packet_run <- function(script, envir = .GlobalEnv) {
  p <- outpack_packet_current()
  assert_relative_path(script, no_dots = TRUE)
  assert_file_exists(script, p$path, "Script")

  ## TODO: Logging

  ## TODO: Control over echo (as in orderly)

  ## TODO: Control over running in separate process (if we do that,
  ## the process should return session, too)

  ## TODO: What should we do/store on error?

  with_dir(p$path, sys.source(script, envir = envir))

  p$script <- c(p$script, script)
  current$packet <- p

  ## What is a good return value here?
  invisible()
}


##' @export
##' @rdname outpack_packet
##'
##' @param id The id of an existing packet to use files from
##'
##' @param files A named character vector of files; the name
##'   corresponds to the name within the current packet, while the
##'   value corresponds to the name within the upstream packet
outpack_packet_use_dependency <- function(id, files) {
  p <- outpack_packet_current()

  meta <- p$root$metadata(id)

  ## Then validation of the request:
  i <- match(files, meta$files$path)
  if (any(is.na(i))) {
    stop(sprintf("file not found in packet '%s' (%s): %s",
                 id, meta$name, paste(files[is.na(i)], collapse = ", ")))
  }

  ## TODO: allow pattern to be used in files (but then how do we
  ## translate to destination?)

  assert_named(files, unique = TRUE)
  assert_relative_path(names(files), no_dots = TRUE)

  depends <- list(id = id,
                  files = data_frame(path = data_frame(path = names(files)),
                                     source = unname(files)))
  p$depends <- c(p$depends, list(depends))

  ## TODO: check that no dependency destination exists, or offer solution
  ## to overwrite.
  dest <- file.path(p$path, names(files))

  ## TODO: log file copy information, including hashes.  Because copy
  ## can be slow for large files, we might want to do this file by
  ## file?

  ## TODO: currently no capacity here for *querying* to find the id
  ## (e.g., latest(name) or something more complex).  Any progress on
  ## this will depend on the query interface.  It's probable that we
  ## might want to record the query here alongside the id, if one was
  ## used?  Or should we allow a query here?

  ## TODO: This copy process should be put into another root method,
  ## as we will do this all over the show, such as in the support for
  ## extracting things from an existing packet anywhere (outside of a
  ## packet build process)

  ## TODO: The copy should ideally all succeed or all fail wherever
  ## possible

  fs::dir_create(dirname(dest))

  if (root$config$core$use_file_store) {
    hash <- meta$files$hash[i]
    for (j in seq_along(i)) {
      root$files$get(hash[[j]], dest[[j]])
    }
  } else {
    ## Here, we should really:
    ## 1. check these exist
    ## 2. check that these have the correct hash (this should be configurable,
    ## with a faster alternative available)
    src <- file.path(root$path, root$config$core$path_archive,
                     meta$name, meta$id, unname(files))
    fs::file_copy(src, dest)
  }

  ## Only update packet information after success, to reflect new
  ## metadata
  current$packet <- p
  invisible()
}


outpack_packet_clear <- function() {
  current$packet <- NULL
}


current <- new.env(parent = emptyenv())
