test_that("can pull metadata from a file base location", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))

  path_upstream <- file.path(tmp, "upstream")
  root_upstream <- outpack_init(path_upstream, use_file_store = TRUE)

  create_packet <- function(root) {
    src <- tempfile()
    on.exit(unlink(src, recursive = TRUE))
    saveRDS(runif(10), file.path(src, "data.rds"))
    id <- outpack_packet_start(src, "data", root = root)$id
    outpack_packet_end()
    id
  }

  ids <- vcapply(1:3, create_packet, file.path(tmp, "src"))

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


test_that("pull metadata from subset of locations", {
  create_packet <- function(root) {
    src <- fs::dir_create(tempfile())
    on.exit(unlink(src, recursive = TRUE))
    saveRDS(runif(10), file.path(src, "data.rds"))
    id <- outpack_packet_start(src, "data", root = root)$id
    outpack_packet_end()
    id
  }

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  path <- root <- list()
  path$a <- file.path(tmp, "a")
  outpack_init(path$a, use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    path[[name]] <- file.path(tmp, name)
    root[[name]] <- outpack_init(path[[name]], use_file_store = TRUE)
    outpack_location_add(name, path[[name]], root = path$a)
  }

  expect_equal(outpack_location_list(root = path$a),
               c("local", "x", "y", "z"))

  ## NOTE: This is a little slow (0.2s) with about half of that coming
  ## from the call to utils::sessionInfo which gets bogged down
  ## reading DESCRIPTION files from disk - we might be better off
  ## replacing that with something a bit simpler. Also seeing some
  ## bottlenecks coming potentially from fs (fs::dir_create - looks
  ## like a known bug)
  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) create_packet(root[[name]]))
  }

  outpack_location_pull_metadata(c("x", "y"), root = path$a)
  index <- outpack_root_open(path$a)$index_update()
  expect_setequal(names(index$metadata), c(ids$x, ids$y))
  expect_equal(index$location$location, rep(c("x", "y"), each = 3))
  expect_equal(index$metadata[ids$x],
               outpack_root_open(path$x)$index_update()$metadata)
  expect_equal(index$metadata[ids$y],
               outpack_root_open(path$y)$index_update()$metadata)

  outpack_location_pull_metadata(root = path$a)
  index <- outpack_root_open(path$a)$index_update()
  expect_setequal(names(index$metadata), c(ids$x, ids$y, ids$z))
  expect_equal(index$location$location, rep(c("x", "y", "z"), each = 3))
  expect_equal(index$metadata[ids$z],
               outpack_root_open(path$z)$index_update()$metadata)
})
