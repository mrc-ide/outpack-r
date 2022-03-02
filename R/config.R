config_new <- function(path_archive, use_file_store) {
  if (!is.null(path_archive)) {
    assert_scalar_character(path_archive)
  }
  assert_scalar_logical(use_file_store)
  if (is.null(path_archive) && !use_file_store) {
    stop("if 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  }
  ## There's a good reason here to wonder if this _should_ be
  ## configurable.  I'll keep it here within the configuration even
  ## though it can't be changed really.
  hash_algorithm <- "sha256"

  cfg <- list(
    schemaVersion = scalar(outpack_schema_version()),
    core = list(
      path_archive = scalar(path_archive),
      use_file_store = scalar(use_file_store),
      hash_algorithm = scalar(hash_algorithm)))
  to_json(cfg)
}
