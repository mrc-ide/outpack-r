## Currently, we're not going to allow any of this to be modified,
## because making a change to the configuration will require that we
## move any checked out files around.
outpack_config_default <- function() {
  to_json(list(
    schemaVersion = scalar(outpack_schema_version()),
    core = list(
      bare = scalar(FALSE),
      hash_algorithm = scalar("sha256"))))
}


## Not sure that we want this tbh...
outpack_config <- function(root, verbose = FALSE) {
  outpack_root_locate(root)$config
}
