test_that("can pull metadata from a file base location", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))

  path_upstream <- file.path(tmp, "upstream")
  root_upstream <- outpack_init(path_upstream, use_file_store = TRUE)
  on.exit(unlink(src, recursive = TRUE), add = TRUE)

  create_packet <- function(i) {
    src <- fs::dir_create(file.path(tmp, "src"))
    saveRDS(runif(10), file.path(src, "data.rds"))
    id <- outpack_packet_start(src, "data", root = path_upstream)$id
    outpack_packet_end()
    id
  }

  src <- fs::dir_create(file.path(tmp, "src"))
  ids <- vcapply(1:3, create_packet)

  path_downstream <- file.path(tmp, "downstream")
  outpack_init(path_downstream, use_file_store = TRUE)

  outpack_location_add("upstream", path_upstream, root = path_downstream)
  expect_equal(outpack_location_list(root = path_downstream),
               c("local", "upstream"))

  outpack_location_pull_metadata("upstream", root = path_downstream)

  ## Sensible tests here will be much easier to write once we have a
  ## decent query interface.

  ## TODO: should not invalidate the existing open root we have, but
  ## we do somehow.
  index <- outpack_root_open(path_downstream)$index_update()
  expect_length(index$metadata, 3)
  expect_setequal(names(index$metadata), ids)
  expect_mapequal(index$metadata, root_upstream$index_update()$metadata)

  expect_s3_class(index$location, "data.frame")
  expect_setequal(index$location$id, ids)
  expect_equal(index$location$location, rep("upstream", 3))
})
