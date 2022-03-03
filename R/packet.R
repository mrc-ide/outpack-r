## Control over an packet as it gets built.  Names here probably want
## changing because we'll need to support interacting with an existing
## packet?

current <- new.env(parent = emptyenv())

## We might allow this function to copy files into a safe place too
outpack_packet_start <- function(path, name, parameters = NULL,
                                 id = NULL, verbose = TRUE, root = NULL) {
  if (is.null(id)) {
    id <- outpack_id()
  }

  root <- outpack_root_locate(root)
  if (!is.null(current$packet)) {
    ## * Could make this root specific?
    ## * Could make this path specific?
    ## * Could use a stack?
    ## * Could automatically exit?
    ## * Could be fine if name/id match
    stop("Already a current packet - call outpack_packet_cancel()")
  }

  time_start <- Sys.time()

  ## We should decide generally what that logging/messaging process
  ## will look like.  So if we have some rich metadata we could pass
  ## that through lgr and convert this into nice cli-based logging I
  ## think.  That will require a little work up front but
  if (verbose) {
    cli::cli_h1("{name} : {id} ({path}):")
    cli::cli_inform("start: {time_start}")
    if (!is.null(parameters)) {
      ## ...etc
    }
  }

  ## TODO: validation on name, id, path, parameters
  ## TODO: optionally scan directory for files?

  current$packet <- list(
    name = name,
    id = id,
    path = path,
    parameters = parameters,
    time_start = time_start,
    root = root,
    verbose = verbose)

  invisible()
}


## These might end up root or path specific.
outpack_packet_current <- function() {
  if (is.null(current$packet)) {
    stop("No current packet")
  }
  current$packet
}


outpack_packet_cancel <- function() {
  if (!is.null(current$packet)) {
    current$packet <- NULL
  }
}


outpack_packet_end <- function() {
  p <- outpack_packet_current()
  p$time_end <- Sys.time()
  if (p$verbose) {
    cli::cli_h2("Finishing packet")
  }
  json <- outpack_metadata_create(p$path, p$name, p$id,
                                  p$depends, p$parameters)
  outpack_insert_packet(p$path, json, p$root, verbose = p$verbose)
  ## Same as with outpack_packet_cancel, probably needs some work to
  ## make this less weird.
  current$packet <- NULL
}


outpack_packet_run <- function(script, envir = .GlobalEnv) {
  ## TODO: Assert that script is a relative path
  ## TODO: Assert that script is found within packet path
  ## TODO: Control over echo (as in orderly)

  ## TODO: Control over running in separate process (if we do that,
  ## the process should return session, too)
  p <- outpack_packet_current()
  withr::with_dir(p$path, sys.source(script, envir = envir))
  ## What is a good return value here?
  invisible()
}


outpack_packet_use_depenency <- function(id, files) {
  p <- outpack_packet_current()

  ## TODO: We need a function that will simply reflect the index, but
  ## we also need something that will allow fetching of metadata
  meta <- p$root$index_update()$metadata[[id]]
  if (is.null(meta)) {
    stop(sprintf("id '%s' not found in index", id))
  }

  ## Then validation of the request:
  i <- match(files, meta$files$path)
  if (any(is.na(i))) {
    stop(sprintf("file not found in packet '%s' (%s): %s",
                 id, meta$name, paste(files[is.na(i)], collapse = ", ")))
  }

  ## TODO: allow pattern to be used in files?
  ## TODO: assert_named(files) # named, uniquely
  ## TODO: check that all names are relative paths with no '..' components
  ## TODO: check that no dependency destination exists

  ## TODO: log this process!

  ## TODO: we need to make it really easy to query versions

  ## Then we look for the content; this should be put into another
  ## root method, as we will do this all over the show.
  dest <- file.path(p$path, names(files))
  fs::dir_create(dirname(dest))

  if (root$config$core$use_file_store) {
    hash <- meta$files$hash[i]
    for (j in seq_along(i)) {
      root$files$get(hash[[j]], dest[[j]])
    }
  } else {
    ## Here, we should really:
    ## 1. check these exist
    ## 2. check that these have the correct hash
    src <- file.path(root$path, root$config$core$path_archive,
                     meta$name, meta$id, unname(files))
    fs::file_copy(src, dest)
  }
}
