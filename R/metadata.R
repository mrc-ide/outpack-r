outpack_metadata_create <- function(path, name, id, time, files = NULL,
                                    depends = NULL, parameters = NULL,
                                    session = NULL,
                                    hash_algorithm = "sha256") {
  assert_scalar_character(name)
  assert_scalar_character(id)

  assert_is(time, "list")
  assert_is(time$start, "POSIXt")
  assert_is(time$end, "POSIXt")
  time$start <- scalar(time_to_num(time$start))
  time$end <- scalar(time_to_num(time$end))

  if (!is.null(parameters)) {
    validate_parameters(parameters)
    parameters <- lapply(parameters, scalar)
  }

  if (is.null(files)) {
    files <- dir(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  } else {
    assert_relative_path(path, no_dots = TRUE)
    assert_file_exists(files, path)
  }

  ## In the most simple case we could just do nothing about inputs vs
  ## outputs; we don't even need a list of them (we just have files
  ## and that's all there is to it).  I am not 100% sure if that's
  ## sensible, but it will be easy enough to extend this later.  For
  ## orderly we can handle this via additional data in 'extra'.  Not
  ## having this distinction will make doing output-only packets
  ## easier of course.
  files <- with_dir(
    path,
    data_frame(
      path = files,
      size = file.size(files),
      hash = vcapply(files, hash_file, hash_algorithm, USE.NAMES = FALSE)))

  ## TODO: best to validate here that all elements of depends are
  ## really found in the inputs list; more generally we might verify
  ## that they really exist at all?

  ## We capture the idea of a single dependency "event" with
  ## potentially several files coming from it.
  if (is.null(depends)) {
    depends <- list()
  } else {
    for (i in seq_along(depends)) {
      depends[[i]]$id <- scalar(depends[[i]]$id)
    }
    ## TODO: Additional checks could be required, but will require a
    ## root.  We do some of these on insert and via
    ## outpack_packet_use_dependency at the moment
    ##
    ## 1. is the id known to the system?
    ## 2. is names(depends[[i]]$files$path) present in 'path' (for all i)?
    ## 3. is unname(depends[[i]]$files$source) present in 'id' (for all i)?
    ## 4. is the file unchanged since import?
    ##
    ## 1, 3 and 4 require that we have the root active as they will
    ## require us to query the index, but we could do '2' here as it
    ## must be consistent within the metadata.
  }

  if (is.null(session)) {
    session <- outpack_session_info(utils::sessionInfo())
  }

  ret <- list(schemaVersion = scalar(outpack_schema_version()),
              name = scalar(name),
              id = scalar(id),
              time = time,
              parameters = parameters,
              files = files,
              depends = depends,
              session = session)

  to_json(ret, "metadata")
}


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


outpack_session_info <- function(info) {
  ## TODO: we might also add some host information here too; orderly
  ## has some of that for us.
  assert_is(info, "sessionInfo")
  platform <- list(version = scalar(info$R.version$version.string),
                   os = scalar(info$running),
                   system = scalar(info$R.version$system))

  ## TODO: Where available, we might also include Remotes info, or
  ## whatever renv uses?
  pkgs <- c(info$otherPkgs, info$loadedOnly)
  n <- c(length(info$otherPkgs), length(info$loadedOnly))
  packages <- data_frame(
    package = vcapply(pkgs, "[[", "Package", USE.NAMES = FALSE),
    version = vcapply(pkgs, "[[", "Version", USE.NAMES = FALSE),
    attached = rep(c(TRUE, FALSE), n))

  list(platform = platform,
       packages = packages)
}


outpack_metadata_index_read <- function(path) {
  keep <- c("name", "id", "parameters", "files", "depends")
  outpack_metadata_load(path)[keep]
}
