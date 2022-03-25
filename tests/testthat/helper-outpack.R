## Globally enable schema validation everywhere; really this should be
## disabled on CRAN, and only enabled if jsonvalidate is found.
options(outpack.schema_validate = TRUE)


create_random_packet <- function(root, name = "data") {
  src <- fs::dir_create(tempfile())
  on.exit(unlink(src, recursive = TRUE))
  saveRDS(runif(10), file.path(src, "data.rds"))
  id <- outpack_packet_start(src, name, root = root)$id
  outpack_packet_end()
  id
}
