##' Start a packet build (`outpack_packet_start`), end one
##' (`outpack_packet_cancel`, `outpack_packet_end`) and interact with
##' one (`outpack_packet_use_dependency`,
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
##' @param id Optionally, an outpack id via [outpack::outpack_id]. If
##'   not given a new id will be generated.
##'
##' @param logging_console Optional logical, indicating if we should
##'   override the root's default in logging to the console. A value
##'   of `NULL` uses the root value, `TRUE` enables console output
##'   even when this is suppressed by the root, and `FALSE` disables
##'   it even when this is enabled by the root.
##'
##' @param logging_threshold Optional log threshold, indicating if we
##'   override the root's default in logging to the console. A value
##'   of `NULL` uses the root value, otherwise use `info`, `debug` or
##'   `trace` (in increasing order of verbosity).
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
                                 logging_console = NULL,
                                 logging_threshold = NULL,
                                 root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)

  assert_scalar_character(name)
  assert_directory(path)
  validate_parameters(parameters)

  if (is.null(id)) {
    id <- outpack_id()
  } else {
    validate_outpack_id(id)
  }

  logger <- outpack_packet_logger(path, root, logging_console,
                                  logging_threshold)
  caller <- "outpack::outpack_packet_start"

  time <- list(start = Sys.time())

  packet <- structure(
    list2env(
      list(
        name = name,
        id = id,
        path = path,
        parameters = parameters,
        files = list(),
        time = time,
        logger = logger,
        root = root),
      parent = emptyenv()),
    class = "outpack_packet")

  outpack_log_info(packet, "name", name, caller)
  outpack_log_info(packet, "id", id, caller)
  if (length(parameters) > 0) {
    detail <- sprintf("%s: %s", names(parameters), unname(parameters))
    outpack_log_info(packet, "parameter", I(detail), caller)
  }
  outpack_log_info(packet, "start", format(time$start), caller)

  invisible(packet)
}


##' @export
##' @rdname outpack_packet
##' @param packet A packet object
outpack_packet_cancel <- function(packet) {
  packet <- check_current_packet(packet)
  outpack_log_info(packet, "cancel", packet$id,
                   "outpack::outpack_packet_cancel")
  outpack_packet_finish(packet)
}


##' @export
##'
##' @param insert Logical, indicating if we should insert the packet
##'   into the store. This is the default and generally what you
##'   want. The use-case we have for `insert = FALSE` is where you
##'   want to write out all metadata after a failure, and in this case
##'   you would not want to do a final insertion into the outpack
##'   archive. When `insert = FALSE`, we write out the json metadata
##'   that would have been written as `outpack.json` within the packet
##'   working directory.  Note that this skips a lot of validation
##'   (for example, validating that all files exist and that files
##'   marked immutable have not been changed)
##'
##' @rdname outpack_packet
outpack_packet_end <- function(packet, insert = TRUE) {
  packet <- check_current_packet(packet)
  packet$time$end <- Sys.time()
  hash_algorithm <- packet$root$config$core$hash_algorithm
  caller <- "outpack::outpack_packet_end"
  outpack_log_info(packet, "end", format(packet$time$end), caller)
  elapsed_str <- format(packet$time$end - packet$time$start)
  outpack_log_info(packet, "elapsed", elapsed_str, caller)
  writeLines(packet$logger$json$get(), file.path(packet$path, "log.json"))
  json <- outpack_metadata_create(packet$path, packet$name, packet$id,
                                  packet$time,
                                  files = NULL,
                                  depends = packet$depends,
                                  parameters = packet$parameters,
                                  script = packet$script,
                                  custom = packet$custom,
                                  session = NULL,
                                  file_hash = packet$files$immutable,
                                  file_ignore = packet$files$ignored,
                                  hash_algorithm = hash_algorithm)
  if (insert) {
    outpack_insert_packet(packet$path, json, packet$root)
  } else {
    ## TODO: I am not sure what the best filename to use here is -
    ## good options feel like 'outpack.json' (like we do with
    ## 'log.json') and `<id>` or '<id>.json' (totally collision
    ## resistant)
    writeLines(json, file.path(packet$path, "outpack.json"))
  }
  outpack_packet_finish(packet)
}


##' @section Running scripts:
##'
##' R does not make it extremely easy to "run" a script while
##'   collecting output and warnings in a nice way; this is something
##'   you may be familiar with when running scripts through things
##'   like knitr where differences in behaviour between running from
##'   within knitr and R are not uncommon.  If you see any behaviour
##'   which feels very different to what you expect please let us
##'   know.
##'
##' One area of known difference is that of warnings; what R does with
##'   warnings depends on a number of options - both global and to
##'   `warning` itself. We do not try very hard currently to get the
##'   same behaviour with warnings as you might see running directly
##'   with `source` and observing your terminal, partly because we
##'   hope that in practice your code will produce very few warnings.
##'
##' On failure in the script, `outpack_packet_run` will throw, forcing
##'   any function that calls `outpack_packet_run` to explicitly cope
##'   with error. The error that is generated will have class
##'   `outpack_packet_run_error` allowing this error to be easily
##'   distinguished from other R errors. It will have, in addition to
##'   a `message` field, additional data fields containing information
##'   about the error:
##'
##' * `error`: the original error object, as thrown and caught by `outpack`
##' * `traceback`: the backtrace for the above error, currently just as a
##'   character vector, though this may change in future versions
##' * `output`: a character vector of interleaved stdout and stderr as
##'   the script ran
##' * `warnings`: a list of warnings raised by the script
##'
##' The other reason why the script may fail is that it fails to
##'   balance one of the global resource stacks - either connections
##'   (rare) or graphics devices (easy to do). In this case, we still
##'   throw a (classed) error, but the `error` field in the final
##'   error will be `NULL`, with an informative message explaining
##'   what was not balanced.
##'
##' @export
##' @rdname outpack_packet
##'
##' @param script Path to the script within the packet directory (a
##'   relative path).  This function can be safely called multiple
##'   times within a single packet run (or zero times!) as needed.
##'
##' @param envir Environment in which to run the script
outpack_packet_run <- function(packet, script, envir = .GlobalEnv) {
  packet <- check_current_packet(packet)
  assert_relative_path(script, no_dots = TRUE)
  assert_file_exists(script, packet$path, "Script")
  caller <- "outpack::outpack_packet_run"

  outpack_log_info(packet, "script", script, caller)

  ## TODO: be careful with nesting; as that complicates the logs and
  ## in particular the sinks.
  result <- evaluate_script(packet$path, script, envir, packet$logger$console)

  status <- if (result$success) "success" else "failure"
  outpack_log_info(packet, "result", status, caller)
  outpack_log_info(packet, "output", I(result$output), caller)
  if (length(result$traceback) > 0) {
    outpack_log_info(packet, "traceback", I(result$traceback), caller)
  }
  if (length(result$warnings) > 0) {
    warnings_str <- vcapply(result$warnings, conditionMessage)
    outpack_log_info(packet, "warning", I(warnings_str), caller)
  }

  packet$script <- c(packet$script, script)

  if (!result$success) {
    class(result) <- c("outpack_packet_run_error", "error", "condition")
    stop(result)
  }

  ## At this point we can only have success so we don't want some
  ## fields; do we even want this though?
  invisible(result[c("output", "warnings")])
}


##' @export
##' @rdname outpack_packet
##'
##' @section Dependency resolution:
##'
##' The `search_options` argument controls where outpack searches for
##'   packets with the given query and if anything might be moved over
##'   the network (or from one outpack archive to another). By default
##'   everything is resolved locally only; that is we can only depend
##'   on packets that are unpacked within our current archive.  If you
##'   pass a `search_options` argument that contains `allow_remote =
##'   TRUE` (see [outpack::outpack_search_options] then packets
##'   that are known anywhere are candidates for using as dependencies
##'   and *if needed* we will pull the resolved files from a remote
##'   location. Note that even if the packet is not locally present
##'   this might not be needed - if you have the same content anywhere
##'   else in an unpacked packet we will reuse the same content
##'   without refetching.
##'
##' If `pull_metadata = TRUE`, then we will refresh location metadata
##'   before pulling, and the `location` argument controls which
##'   locations are pulled from.
##'
##' @param query An [outpack::outpack_query] object, or something
##'   (e.g., a string) that can be trivially converted into one.
##'
##' @param files A named character vector of files; the name
##'   corresponds to the name within the current packet, while the
##'   value corresponds to the name within the upstream packet
##'
##' @param search_options Optional search options for restricting the
##'   search (see [outpack::outpack_search] for details)
outpack_packet_use_dependency <- function(packet, query, files,
                                          search_options = NULL) {
  packet <- check_current_packet(packet)
  query <- as_outpack_query(query)
  search_options <- as_outpack_search_options(search_options)

  if (!query$info$single) {
    stop(paste(
      "The provided query is not guaranteed to return a single value:",
      squote(deparse_query(query$value$expr)),
      "Did you forget latest(...)?"))
  }

  id <- outpack_search(query, parameters = packet$parameters,
                       options = search_options, root = packet$root)
  if (is.na(id)) {
    ## TODO: this is where we would want to consider explaining what
    ## went wrong; because that comes with a cost we should probably
    ## control this behind some options. We also would want to return
    ## some classed error here to help with formatting and handling
    ## the error. In particular we definitely want to allow for
    ## cycling through allow_remote and location in addition to
    ## near misses on parameters etc.
    stop(sprintf("Failed to find packet for query:\n    %s", format(query)))
  }

  needs_pull <- search_options$allow_remote &&
    packet$root$config$core$require_complete_tree &&
    !(id %in% packet$root$index()$unpacked)
  if (needs_pull) {
    outpack::outpack_location_pull_packet(id, search_options$location,
                                          root = packet$root)
  }

  outpack_copy_files(id, files, packet$path,
                     allow_remote = search_options$allow_remote,
                     root = packet$root)

  query_str <- deparse_query(query$value$expr,
                             lapply(query$subquery, "[[", "expr"))

  ## Only update packet information after success, to reflect new
  ## metadata
  depends <- list(
    packet = id,
    query = query_str,
    files = data_frame(here = names(files), there = unname(files)))
  packet$depends <- c(packet$depends, list(depends))

  outpack_packet_file_mark(packet, names(files), "immutable")

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
outpack_packet_add_custom <- function(packet, application, data,
                                      schema = NULL) {
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

  if (application %in% vcapply(packet$custom, "[[", "application")) {
    stop(sprintf("metadata for '%s' has already been added for this packet",
                 application))
  }

  custom <- list(application = application, data = data)
  packet$custom <- c(packet$custom, list(custom))
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
##' @param packet Optionally, an explicitly-passed packet; see
##'   [outpack::outpack_packet_start()] for details.
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
outpack_packet_file_mark <- function(packet, files, status) {
  status <- match_value(status, c("immutable", "ignored"))
  packet <- check_current_packet(packet)

  assert_relative_path(files, no_dots = TRUE)
  assert_file_exists(files, packet$path)

  ## TODO: these are exclusive categories because we later return a
  ## 1:1 mapping of file to status
  if (status == "immutable") {
    hash_algorithm <- packet$root$config$core$hash_algorithm
    value <- with_dir(packet$path,
                      hash_files(files, hash_algorithm, named = TRUE))

    if (any(files %in% packet$files$ignored)) {
      stop(sprintf("Cannot mark ignored files as immutable: %s",
                   paste(squote(intersect(files, packet$files$ignored)),
                         collapse = ", ")))
    }

    prev <- names(packet$files$immutable)
    validate_hashes(value, packet$files$immutable[intersect(files, prev)])
    value <- value[setdiff(names(value), prev)]
    packet$files$immutable <- c(packet$files$immutable, value)
  } else if (status == "ignored") {
    if (any(files %in% names(packet$files$immutable))) {
      stop(sprintf(
        "Cannot mark immutable files as ignored: %s",
        paste(squote(intersect(files, names(packet$files$immutable))),
              collapse = ", ")))
    }

    packet$files$ignored <- union(packet$files$ignored, files)
  }
  invisible()
}


##' @export
##' @rdname outpack_packet_file
outpack_packet_file_list <- function(packet) {
  packet <- check_current_packet(packet)
  files <- with_dir(packet$path,
                    dir(all.files = TRUE, recursive = TRUE, no.. = TRUE))
  status <- rep("unknown", length(files))
  status[files %in% names(packet$files$immutable)] <- "immutable"
  status[files %in% packet$files$ignored] <- "ignored"
  data_frame(path = files, status = status)
}



outpack_packet_finish <- function(packet) {
  packet$complete <- TRUE
}


## This is used in each of the functions that accept either 'packet'
## as an argument and which will fall back onto the global active
## packet.
check_current_packet <- function(packet) { # TODO: rename
  assert_is(packet, "outpack_packet")
  if (isTRUE(packet$complete)) {
    stop(sprintf("Packet '%s' is complete", packet$id))
  }
  packet
}
