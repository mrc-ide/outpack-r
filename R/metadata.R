##' Create outpack metadata.
##'
##' @title Create outpack metadata
##'
##' @param path Directory holding the packet
##'
##' @param name The name of the packet
##'
##' @param id The unique identifier
##'
##' @param time A list of times. Must contain the elements `start` and `end` (as `POSIXt` objects) and may contain any
##'
##' @param inputs A character vector of inputs
##'
##' @param outputs A character vector of outputs
##'
##' @param depends Optionally, information about dependencies on other
##'   packets.  This must a list of objects created by
##'   `outpack::outpack_metadata_depends`, and all files referenced
##'   should be found within `inputs`
##'
##' @param parameters Optionally, a list of named key/value
##'   parameters.  Each key must be string and each value may be a
##'   boolean, number or string (no vectors are allowed).
##'
##' @param session Optionally, information about the running session.
##'   If omitted, then default session information is included.
##'
##' @param extra Optionally, arbitrary additional metadata as needed.
##'   This will be passed through `jsonlite::toJSON` with arguments
##'   `pretty = FALSE, auto_unbox = FALSE, json_verbatim = TRUE, na =
##'   "null", null = "null"`, so plan accordingly.
##'
##' @param hash_algorithm Optionally, the hash algorithm to use.  The
##'   default is `sha256` which is what git uses and should be good
##'   for most uses.
##'
##' @return A `json` string
##'
##' @export
outpack_metadata_create <- function(path, name, id, time,
                                    inputs, outputs, depends = NULL,
                                    parameters = NULL, session = NULL,
                                    extra = NULL, hash_algorithm = "sha256") {
  owd <- setwd(path)
  on.exit(setwd(owd))

  assert_scalar_character(name)
  assert_scalar_character(id)

  ## Here, we will probably tweak this a little later, this validation
  ## is not great as it would struggle to properly sanitise other
  ## information here (e.g., numbers)
  assert_is(time, "list")
  assert_is(time$start, "POSIXt")
  assert_is(time$end, "POSIXt")

  time$elapsed <- scalar(as.numeric(time$end - time$start, units = "secs"))
  for (v in names(time)) {
    if (inherits(time[[v]], "POSIXt")) {
      time[[v]] <- scalar(as.character(time[[v]]))
    }
  }

  ## inputs and outputs must be *relative* paths, within 'path'. Not
  ## yet asserted...

  if (!is.null(inputs)) {
    assert_character(inputs)
    assert_relative_path(inputs)
    inputs <- unname(lapply(inputs, outpack_metadata_file, hash_algorithm))
  }

  if (length(outputs) == 0) {
    stop("At least one 'outputs' is required")
  }
  assert_character(outputs)
  assert_relative_path(outputs)
  outputs <- unname(lapply(outputs, outpack_metadata_file, hash_algorithm))

  ## TODO: best to validate here that all elements of depends are
  ## really found in the inputs list.
  if (!all(vlapply(depends, inherits, "outpack_metadata_depends"))) {
    stop("All elements of 'depends' must be 'outpack_metadata_depends'")
  }
  depends <- lapply(depends, drop_class)

  if (!is.null(parameters)) {
    assert_named(parameters)
    ## TODO: check basic types here, ensure scalar
    parameters <- lapply(parameters, scalar)
  }

  if (is.null(session)) {
    session <- outpack_session_info(utils::sessionInfo())
  }

  ## TODO: make sure that zero length inputs, depends are actually
  ## NULL; the 'all' conditions would be true for integer(0) etc.
  ret <- list(schemaVersion = scalar(outpack_schema_version()),
              name = scalar(name),
              id = scalar(id),
              parameters = parameters,
              time = time,
              inputs = inputs,
              outputs = outputs,
              depends = depends,
              session = session)

  if (!is.null(extra)) {
    assert_named(extra)
    ret <- c(ret, extra)
  }

  ## We *must* use pretty = FALSE because we might sign this later.
  jsonlite::toJSON(ret, pretty = FALSE, auto_unbox = FALSE,
                   json_verbatim = TRUE, na = "null", null = "null")
}


##' Load outpack metadata as an R object.  Note that due to the
##' difficulties of serialising R objects to JSON this cannot be
##' directly re-converted to JSON (but as the outpack metadata is
##' immutable, that should not be a big limitation.
##'
##' @title Load outpack metadata
##'
##' @param json A path to generated json, or the json itself as a
##'   string.
##'
##' @return A list with the deserialised metadata
##'
##' @export
outpack_metadata_load <- function(json) {
  if (inherits(json, "json")) {
    jsonlite::parse_json(json)
  } else {
    jsonlite::read_json(json)
  }
}


##' Validate metadata against the schema
##'
##' @title Validate metadata against schema
##'
##' @inheritParams outpack_metadata_load
##'
##' @return Logical `TRUE` (invisibly) if valid, error otherwise.
##'
##' @export
outpack_metadata_validate <- function(json) {
  invisible(outpack_metadata_schema()$validate(json, error = TRUE))
}


outpack_schema_version <- function() {
  if (is.null(cache$schema)) {
    path <- outpack_file("schema/outpack.json")
    cache$schema_version <- jsonlite::read_json(path)$version
  }
  cache$schema_version
}


outpack_metadata_schema <- function() {
  if (is.null(cache$schema)) {
    path <- outpack_file("schema/outpack.json")
    cache$schema <- jsonvalidate::json_schema$new(path)
  }
  cache$schema
}


outpack_metadata_file <- function(path, hash_algorithm) {
  if (!file.exists(path)) {
    stop("File missing")
  }
  list(path = scalar(path),
       size = scalar(file.size(path)),
       hash = scalar(hash_file(path, hash_algorithm)))
}


outpack_metadata_depends <- function(id, name, files) {
  assert_scalar_character(id) # TODO: check format matches here
  assert_scalar_character(name)
  assert_named(files)
  assert_character(files)
  ret <- list(id = scalar(id),
              name = scalar(name),
              files = lapply(files, scalar))
  class(ret) <- c("outpack_metadata_depends", "outpack_metadata_partial")
  ret
}


outpack_session_info <- function(info) {
  platform <- list(version = scalar(info$R.version$version.string),
                   os = scalar(info$running),
                   system = scalar(info$R.version$system))

  pkg_info <- function(el, attached) {
    list(package = scalar(el$Package),
         version = scalar(el$Version),
         attached = scalar(attached))
  }
  packages <- unname(c(lapply(info$otherPkgs, pkg_info, TRUE),
                       lapply(info$loadedOnly, pkg_info, FALSE)))

  list(platform = platform,
       packages = packages)
}
