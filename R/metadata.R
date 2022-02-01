outpack_schema_version <- function() {
  if (is.null(cache$schema)) {
    path <- outpack_file("schema/outpack.json")
    cache$schema_version <- jsonlite::read_json(path)$version
  }
  cache$schema_version
}


outpack_metadata_validate <- function(path) {
  outpack_metadata_schema()$validate(path, error = TRUE)
}


outpack_metadata_schema <- function() {
  if (is.null(cache$schema)) {
    path <- outpack_file("schema/outpack.json")
    cache$schema <- jsonvalidate::json_schema$new(path)
  }
  cache$schema
}


outpack_metadata_read <- function(path) {
  jsonlite::read_json(path)
}


outpack_metadata_create <- function(name, id, time, inputs, outputs,
                                    depends = NULL, parameters = NULL,
                                    session = NULL, extra = NULL,
                                    pretty = FALSE) {
  assert_scalar_character(name)
  assert_scalar_character(id)
  assert_is(time, "outpack_metadata_time")
  time <- drop_class(time)

  if (!all(vlapply(inputs, inherits, "outpack_metadata_file"))) {
    stop("All elements of 'inputs' must be 'outpack_metadata_file'")
  }
  inputs <- lapply(inputs, drop_class)
  if (!all(vlapply(outputs, inherits, "outpack_metadata_file"))) {
    stop("All elements of 'outputs' must be 'outpack_metadata_file'")
  }
  outputs <- lapply(outputs, drop_class)
  if (length(outputs) == 0) {
    stop("At least one 'outputs' is required")
  }
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
    session <- outpack_session_info(sessionInfo())
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

  jsonlite::toJSON(ret, pretty = pretty)
}


outpack_metadata_time <- function(begin, end) {
  ## There's a question here about what we should do for timezones
  assert_is(begin, "POSIXt")
  assert_is(end, "POSIXt")
  ret <- list(begin = scalar(as.character(begin)),
              end = scalar(as.character(end)),
              elapsed = scalar(as.numeric(end - begin, units = "secs")))
  class(ret) <- c("outpack_metadata_time", "outpack_metadata_partial")
  ret
}


outpack_metadata_file <- function(path, hash, size, algorithm) {
  if (!file.exists(path)) {
    stop("File missing")
  }
  if (is.null(size)) {
    size <- file.size(path)
  } else if (size != file.size(path)) {
      stop(sprintf(
        "Unexpected file size for %s:\n - expected: %s\n - found:    %s",
        path, size, file.size(path)))
  }
  if (!is.null(hash)) {
    assert_scalar_character(hash)
    expected <- parse_hash(hash)
    found <- hash_file(path, expected$algorithm)
    if (found != hash) {
      stop(sprintf(
        "Unexpected %s hash for %s:\n - expected: %s\n - found:    %s",
        expected$algorithm, path, expected$value, parse_hash(found)$value))

    }
  }
  hash <- hash_file(path, algorithm)

  ret <- list(path = scalar(path),
              hash = scalar(hash),
              size = scalar(size))
  class(ret) <- c("outpack_metadata_file", "outpack_metadata_input")
  ret
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


outpack_session_info <- function(info = sessionInfo()) {
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
