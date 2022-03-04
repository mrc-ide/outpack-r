outpack_schema_version <- function() {
  if (is.null(cache$schema)) {
    path <- outpack_file("schema/metadata.json")
    cache$schema_version <- jsonlite::read_json(path)$version
  }
  cache$schema_version
}


outpack_schema <- function(name) {
  if (is.null(cache$schema[[name]])) {
    if (is.null(cache$schema)) {
      cache$schema <- list()
    }
    message(sprintf("Validating (%s)", name))
    path <- outpack_file(sprintf("schema/%s.json", name))
    cache$schema[[name]] <- jsonvalidate::json_schema$new(path)
  }
  cache$schema[[name]]
}
