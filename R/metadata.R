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
##' @param files A character of files. If `NULL`, then all files
##'   within `path` are considered part of the packet.
##'
##' @param inputs A character vector of inputs
##'
##' @param outputs A character vector of outputs
##'
##' @param depends Optionally, information about dependencies on other
##'   packets.  This must a list of objects created by
##'   [outpack::outpack_metadata_depends], and all files referenced
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
outpack_metadata_create <- function(path, name, id, files = NULL,
                                    depends = NULL, parameters = NULL,
                                    session = NULL, extra = NULL,
                                    hash_algorithm = "sha256") {
  owd <- setwd(path)
  on.exit(setwd(owd))

  assert_scalar_character(name)
  assert_scalar_character(id)

  if (!is.null(parameters)) {
    assert_named(parameters)
    ## TODO: check basic types here, ensure scalar
    parameters <- lapply(parameters, scalar)
  }

  if (is.null(files)) {
    ## NOTE: look in current directory because of the setwd above.
    files <- dir(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  } else {
    ## TODO: make sure that all files are inside of this directory
    ## (and are relative paths)
  }

  ## In the most simple case we could just do nothing about inputs vs
  ## outputs; we don't even need a list of them (we just have files
  ## and that's all there is to it).  I am not 100% sure if that's
  ## sensible, but it will be easy enough to extend this later.  For
  ## orderly we can handle this via additional data in 'extra'.  Not
  ## having this distinction will make doing output-only packets
  ## easier of course.
  files <- data_frame(
    path = files,
    size = fs::file_size(files),
    hash = vcapply(files, hash_file, hash_algorithm))

  ## TODO: best to validate here that all elements of depends are
  ## really found in the inputs list; more generally we might verify
  ## that they really exist at all?

  ## What *IS* the best way of modelling this? I think that capturing
  ## the idea of a single dependency "event" is important, because
  ## that's what we'd end up hanging a query against
  if (is.null(depends)) {
    depends <- list()
  } else {
    if (inherits(depends, "outpack_metadata_depends")) {
      depends <- list(drop_class(depends))
    } else {
      assert_is(depends, "list")
      if (!all(vlapply(depends, inherits, "outpack_metadata_depends"))) {
        stop("All elements of 'depends' must be 'outpack_metadata_depends'")
      }
      depends <- unname(lapply(depends, drop_class))
    }

    ## TODO: Additional checks are required here:
    ##
    ## 1. is the id known to the system?
    ## 2. is names(depends[[i]]$files$path) present in 'path' (for all i)?
    ## 3. is unname(depends[[i]]$files$source) present in 'id' (for all i)?
    ## 4. is the file unchanged since import?
    ##
    ## 1, 3 and 4 require that we have the root active as they will
    ## require us to query the index.
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
              files = files,
              depends = depends,
              session = session)

  if (!is.null(extra)) {
    assert_named(extra)
    ret <- c(ret, extra)
  }

  to_json(ret)
}


##' @rdname outpack_metadata_create
##'
##' @param files A named character vector of files; the name
##'   corresponds to the name within the current packet, while the
##'   value corresponds to the name within the upstream packet
##'
##' @export
outpack_metadata_depends <- function(id, files) {
  assert_scalar_character(id) # TODO: check format matches here
  assert_named(files)
  assert_character(files)
  ret <- list(id = scalar(id),
              files = data_frame(path = names(files),
                                 source = unname(files)))
  class(ret) <- "outpack_metadata_depends"
  ret
}


##' Load outpack metadata as an R object.  Note that due to the
##' difficulties of serialising R objects to JSON this cannot be
##' directly re-converted to JSON (but as the outpack metadata is
##' immutable, that should not be a big limitation.
##'
##' @title Load outpack metadata
##'
##' @param json A path to generated json, or the json itself as a
##'   string (must be of class 'json')
##'
##' @return A list with the deserialised metadata
##'
##' @export
outpack_metadata_load <- function(json) {
  if (!inherits(json, "json")) { # could use starts with "{"
    json <- read_string(json)
  }

  data <- jsonlite::parse_json(json)
  data$hash <- hash_data(json, "sha256")
  data$files <- data_frame(path = vcapply(data$files, "[[", "path"),
                           size = vnapply(data$files, "[[", "size"),
                           hash = vcapply(data$files, "[[", "hash"))
  data$depends <- data.frame(
    id = vcapply(data$depends, "[[", "id"),
    files = I(lapply(data$depends, function(x)
      data_frame(path = vcapply(x$files, "[[", "path"),
                 source = vcapply(x$files, "[[", "source")))))

  data
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


##' Create a reasonable `session` entry using output from [sessionInfo()]
##'
##' @title Create session information
##'
##' @param info Output from [sessionInfo()]
##'
##' @return A list with information on the platform and packages
##'
##' @export
##' @examples
##' dat <- outpack::outpack_session_info(sessionInfo())
##' jsonlite::toJSON(dat, pretty = TRUE)
outpack_session_info <- function(info) {
  ## TODO: we might also add some host information here too; orderly
  ## has some of that for us.
  assert_is(info, "sessionInfo")
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


outpack_schema_version <- function() {
  if (is.null(cache$schema)) {
    path <- outpack_file("schema/metadata.json")
    cache$schema_version <- jsonlite::read_json(path)$version
  }
  cache$schema_version
}


outpack_metadata_schema <- function() {
  if (is.null(cache$schema)) {
    path <- outpack_file("schema/metadata.json")
    cache$schema <- jsonvalidate::json_schema$new(path)
  }
  cache$schema
}


outpack_metadata_file <- function(path, hash_algorithm) {
  assert_file_exists(path)
  list(path = scalar(path),
       size = scalar(file.size(path)),
       hash = scalar(hash_file(path, hash_algorithm)))
}


outpack_metadata_index_read <- function(path) {
  keep <- c("name", "id", "parameters", "files", "depends")
  outpack_metadata_load(path)[keep]
}
