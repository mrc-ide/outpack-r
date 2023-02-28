##' Start a packet build (`outpack_packet_start`), end one
##' (`outpack_packet_cancel`, `outpack_packet_end`) and interact with
##' the current packet (`outpack_packet_use_dependency`,
##' `outpack_packet_run`)
##'
##' There are two ways of using these functions - normally you call
##' `outpack_packet_start`, which registers an "active" packet,
##' following this all other functions (e.g., `outpack_packet_run`)
##' use this active packet. The other approach is to take the return
##' value from `outpack_packet_start` and pass that through as the
##' `packet` argument to any other function.
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
##' @param local Logical, indicating if this should be considered a
##'   "local" packet and not set as the default for other packet
##'   functions. If `TRUE`, then you must explicitly pass the packet
##'   (the return value from `outpack_packet_start`) into each of the
##'   other packet functions (e.g, `outpack_packet_run` and
##'   `outpack_packet_end`)
##'
##' @param id Optionally, an outpack id via [outpack::outpack_id]. If
##'   not given a new id will be generated.
##'
##' @param root The outpack root. Will be searched for from the
##'   current directory if not given.
##'
##' @return Invisibly, a copy of the packet data; this can be passed
##'   as the `packet` argument.
##'
##' @rdname outpack_packet
##' @export
outpack_packet_start <- function(path, name, parameters = NULL, id = NULL,
                                 local = FALSE, root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  assert_scalar_logical(local)
  if (!is.null(current$packet) && !local) {
    ## * Could make this root specific?
    stop(paste("Already a current packet - call outpack_packet_cancel()",
               "or use local = TRUE for an isolated packet"))
  }

  assert_scalar_character(name)
  assert_directory(path)
  validate_parameters(parameters)

  if (is.null(id)) {
    id <- outpack_id()
  } else {
    validate_outpack_id(id)
  }

  time <- list(start = Sys.time())

  ## LOGGING: name / id / path, start time, parameters
  ##
  ## We log these all in orderly and that's super useful
  packet <- structure(
    list2env(
      list(
        name = name,
        id = id,
        path = path,
        parameters = parameters,
        files = list(),
        time = time,
        root = root),
      parent = emptyenv()),
    class = "outpack_packet")

  if (!local) {
    current$packet <- packet
  }

  invisible(packet)
}


##' @export
##' @rdname outpack_packet
##' @param packet Optionally, an explicitly-passed packet; see Details
outpack_packet_cancel <- function(packet = NULL) {
  p <- check_current_packet(packet)
  outpack_packet_finish(p)
}


##' @export
##' @rdname outpack_packet
outpack_packet_current <- function() {
  check_current_packet(NULL)
}


##' @export
##' @rdname outpack_packet
outpack_packet_end <- function(packet = NULL) {
  p <- check_current_packet(packet)
  p$time$end <- Sys.time()
  hash_algorithm <- p$root$config$core$hash_algorithm
  json <- outpack_metadata_create(p$path, p$name, p$id, p$time,
                                  files = NULL,
                                  depends = p$depends,
                                  parameters = p$parameters,
                                  script = p$script,
                                  custom = p$custom,
                                  session = NULL,
                                  file_hash = p$files$immutable,
                                  file_ignore = p$files$ignored,
                                  hash_algorithm = hash_algorithm)
  outpack_insert_packet(p$path, json, p$root)
  outpack_packet_finish(p)
}


##' @export
##' @rdname outpack_packet
##'
##' @param script Path to the script within the packet directory (a
##'   relative path).  This function can be safely called multiple
##'   times within a single packet run (or zero times!) as needed.
##'
##' @param envir Environment in which to run the script
##'
##' @param echo Print the result of running the R code to the
##'   console. This may be difficult to suppress in some context, as
##'   it comes directly from R's [source] function.
outpack_packet_run <- function(script, envir = .GlobalEnv, echo = FALSE,
                               packet = NULL) {
  p <- check_current_packet(packet)
  assert_relative_path(script, no_dots = TRUE)
  assert_file_exists(script, p$path, "Script")

  ## TODO: not sure that this is the correct environment; should it be
  ## parent.frame() perhaps (see default args to new.env)

  ## TODO: Logging; if we are logging then should we also echo to log
  ## too? In addition to the console or instead, and what controls
  ## that?

  ## TODO: Control over running in separate process (if we do that,
  ## the process should return session, too). This is probably a bit
  ## hard to get right as we'd need to know what bits of the session
  ## need replaying into the second session - I suspect it should be
  ## an entirely different function. More likely we'll run the whole
  ## packet setup in a new process as we currently do in orderly.

  ## TODO: What should we do/store on error?

  info <- outpack_packet_run_global_state()

  ## It's important to do the global state check in the packet working
  ## directory (not the calling working directory) otherwise we might
  ## write out files in unexpected places when flushing devices.
  with_dir(p$path, {
    source_script(script, envir, echo)
    outpack_packet_run_check_global_state(info)
  })

  p$script <- c(p$script, script)

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
outpack_packet_use_dependency <- function(id, files, packet = NULL) {
  p <- check_current_packet(packet)
  root <- p$root

  ## TODO: currently no capacity here for *querying* to find the id
  ## (e.g., latest(name) or something more complex).  Any progress on
  ## this will depend on the query interface.  It's probable that we
  ## might want to record the query here alongside the id, if one was
  ## used?  Or should we allow a query here?

  ## TODO: allow pattern to be used in files (but then how do we
  ## translate to destination?)
  assert_named(files, unique = TRUE)
  assert_relative_path(names(files), no_dots = TRUE)

  src <- unname(files)
  dst <- file.path(p$path, names(files))

  ## Ensures that we can actually find all the files within this
  ## dependency (and that it exists)
  validate_packet_has_file(root, id, src)

  ## Actually copy the files into the requested directory
  file_export(root, id, src, dst)

  ## Only update packet information after success, to reflect new
  ## metadata

  depends <- list(packet = id,
                  files = data_frame(here = names(files), there = src))
  p$depends <- c(p$depends, list(depends))

  invisible()
}


##' @section Custom metadata:
##'
##' The `outpack_packet_add_custom` function adds arbitrary
##'   additional metadata into a packet. It is primarily designed for
##'   use with applications that build on outpack to provide
##'   additional information beyond the minimal set provided by
##'   outpack.
##'
##' For example, orderly tracks "artefacts" which collect groups of
##'   file outputs into logical bundles.  To support this it needs to
##'   register additional data for each artefact with:
##'
##' * the description of the artefect (a short phrase)
##' * the format of the artefact (a string describing the data type)
##' * the contents of the artefact (an array of filenames)
##'
##' JSON for this might look like:
##'
##' ```json
##' {
##'   "artefacts": [
##'     {
##'       "description": "Data for onward use",
##'       "format": "data",
##'       "contents": ["results.rds", "summary.rds"]
##'     },
##'     {
##'       "description": "Diagnostic figures",
##'       "format": "staticgraph",
##'       "contents": ["fits.png", "inputs.png"]
##'     }
##'   ]
##' }
##' ```
##'
##' Here, we describe two artefacts, together collecting four files.
##'
##' We need to store these in outpack's final metadata, and we want to
##'   do this in a way that allows easy querying later on while
##'   scoping the data to your application.  To allow for this we
##'   group all data your application adds under an application key
##'   (e.g., `orderly`).  You can then store whatever data you want
##'   beneath this key.
##'
##' **NOTE1**: A limitation here is that the filenames above cannot be
##'   checked against the outpack list of files because outpack does
##'   not know that `contents` here refers to filenames.
##'
##' **NOTE2**: To allow for predictable serialisation to JSON, you
##'   must serialise your own data before passing through to
##'   `outpack_packet_add_custom`.
##'
##' @export
##' @rdname outpack_packet
##'
##' @param application The name of the application (used to organise
##'   the data and query it later, see Details)
##'
##' @param data Additional metadata to add to the packet. This must be
##'   a string representing already-serialised json data.
##'
##' @param schema Optionally, but recommended, a schema to validate
##'   `data` against.  Validation will only happen if the option
##'   `outpack.schema_validate` is `TRUE`, as for the main schema
##'   validation.  Will be passed to [jsonvalidate::json_schema], so
##'   can be a string containing the schema or a path to the schema.
outpack_packet_add_custom <- function(application, data, schema = NULL,
                                      packet = NULL) {
  p <- check_current_packet(packet)

  assert_scalar_character(application)
  assert_scalar_character(data)
  if (!is.null(schema)) {
    assert_scalar_character(data)
  }

  tryCatch(
    jsonlite::parse_json(data),
    error = function(e) {
      stop("Syntax error in custom metadata: ", e$message, call. = FALSE)
    })

  if (should_validate_schema(schema)) {
    tryCatch(
      custom_schema(schema)$validate(data, error = TRUE),
      error = function(e) {
        stop("Validating custom metadata failed: ", e$message, call. = FALSE)
      })
  }

  if (application %in% vcapply(p$custom, "[[", "application")) {
    stop(sprintf("metadata for '%s' has already been added for this packet",
                 application))
  }

  custom <- list(application = application, data = data)
  p$custom <- c(p$custom, list(custom))
  invisible()
}


##' Mark file within an in-progress packet. This will store the hash
##' of the file within the internal outpack structures and force an
##' error if the file is changed or deleted later.  The function
##' [outpack::outpack_packet_file_list()] will report on which files
##' are marked (or unmarked) within the directory.
##'
##' @title Mark files during packet run
##'
##' @param files A character vector of relative paths
##'
##' @param status A status to mark the file with. Must be "immutable"
##'   or "ignored"
##'
##' @return Depending on function
##'
##' * `outpack_packet_file_mark` returns nothing
##' * `outpack_packet_file_list` returns a [data.frame] with columns
##'   `path` and `status` (`immutable`, `ignored` or `unknown`)
##'
##' @rdname outpack_packet_file
##'
##' @export
outpack_packet_file_mark <- function(files, status, packet = NULL) {
  status <- match_value(status, c("immutable", "ignored"))
  p <- check_current_packet(packet)

  assert_relative_path(files, no_dots = TRUE)
  assert_file_exists(files, p$path)

  ## TODO: these are exclusive categories because we later return a
  ## 1:1 mapping of file to status
  if (status == "immutable") {
    hash_algorithm <- p$root$config$core$hash_algorithm
    value <- with_dir(p$path, hash_files(files, hash_algorithm, named = TRUE))

    if (any(files %in% p$files$ignored)) {
      stop(sprintf("Cannot mark ignored files as immutable: %s",
                   paste(squote(intersect(files, p$files$ignored)),
                         collapse = ", ")))
    }
    if (any(files %in% names(p$files$immutable))) {
      validate_hashes(value, p$files$immutable)
      value <- value[!(names(value) %in% names(p$files))]
    }

    p$files$immutable <- c(p$files$immutable, value)
  } else if (status == "ignored") {
    if (any(files %in% names(p$files$immutable))) {
      stop(sprintf("Cannot mark immutable files as ignored: %s",
                   paste(squote(intersect(files, names(p$files$immutable))),
                         collapse = ", ")))
    }

    p$files$ignored <- union(p$files$ignored, files)
  }
  invisible()
}


##' @export
##' @rdname outpack_packet_file
outpack_packet_file_list <- function(packet = NULL) {
  p <- check_current_packet(packet)
  files <- with_dir(p$path,
                    dir(all.files = TRUE, recursive = TRUE, no.. = TRUE))
  status <- rep("unknown", length(files))
  status[files %in% names(p$files$immutable)] <- "immutable"
  status[files %in% p$files$ignored] <- "ignored"
  data_frame(path = files, status = status)
}



## Mostly used in tests as an unconditional "remove any packet with no
## chance of failure" (vs outpack_packet_finish, which does a more
## careful tidyup)
outpack_packet_clear <- function() {
  if (!is.null(current$packet)) {
    current$packet$complete <- TRUE
    current$packet <- NULL
  }
}


outpack_packet_finish <- function(packet) {
  packet$complete <- TRUE
  if (identical(packet, current$packet)) {
    current$packet <- NULL
  }
}


## This is used in each of the functions that accept either 'packet'
## as an argument and which will fall back onto the global active
## packet.
check_current_packet <- function(packet) {
  if (is.null(packet)) {
    packet <- current$packet
    if (is.null(packet)) {
      stop("No currently active packet")
    }
  } else {
    assert_is(packet, "outpack_packet")
    if (isTRUE(packet$complete)) {
      stop(sprintf("Packet '%s' is complete", packet$id))
    }
  }
  packet
}


outpack_packet_run_global_state <- function() {
  list(n_open_devices = length(grDevices::dev.list()),
       n_open_sinks = sink.number())
}


outpack_packet_run_check_global_state <- function(info) {
  check_device_stack(info$n_open_devices)
  check_sink_stack(info$n_open_sinks)
}


current <- new.env(parent = emptyenv())
