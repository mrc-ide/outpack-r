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
