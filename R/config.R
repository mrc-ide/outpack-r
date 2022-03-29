outpack_config_set <- function(..., options = list(...), root = NULL) {
  root <- outpack_root_locate(root)
  assert_named(options, unique = TRUE)
  nms <- names(options)
  valid <- "core.require_pull_recursive"
  unknown <- setdiff(nms, valid)
  if (length(unknown)) {
    stop("Can't set configuration option: ",
         paste(squote(unknown), collapse = ", "))
  }

}


config_set_require_pull_recursive <- function(root, value) {
  if (root$config$core$require_pull_recursive == value) {
    message("'core.require_pull_recursive' was unchanged")
    return()
  }

  config <- root$config

  if (value) {
    ## Here, we need to make sure that we can fulfil all packets by
    ## pulling any referenced but missing packets.  This would be
    ## nicest to do from "any" location, following the chain of trust.
    browser()
  }

  config$core$require_pull_recursive <- value
  config_write(config, root$path)
  root$config <- config
}



config_new <- function(path_archive, use_file_store, require_pull_recursive) {
  if (!is.null(path_archive)) {
    assert_scalar_character(path_archive)
  }
  assert_scalar_logical(use_file_store)
  if (is.null(path_archive) && !use_file_store) {
    stop("if 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  }

  assert_scalar_logical(require_pull_recursive)

  ## TODO: There's a good reason here to wonder if this _should_ be
  ## configurable.  I'll keep it here within the configuration even
  ## though it can't be changed really.
  hash_algorithm <- "sha256"

  list(
    schemaVersion = outpack_schema_version(),
    core = list(
      path_archive = path_archive,
      use_file_store = use_file_store,
      require_pull_recursive = require_pull_recursive,
      hash_algorithm = hash_algorithm),
    location = list())
}


config_serialise <- function(config, path) {
  config$schemaVersion <- scalar(config$schemaVersion) # nolint
  config$core <- lapply(config$core, scalar)
  config$location <- lapply(unname(config$location), lapply, scalar)
  to_json(config, "config")
}


config_write <- function(config, root_path) {
  writeLines(config_serialise(config),
             file.path(root_path, ".outpack", "config.json"))
}


config_read <- function(root_path) {
  config <- jsonlite::read_json(file.path(root_path, ".outpack/config.json"))
  names(config$location) <- vcapply(config$location, "[[", "name")
  config
}
