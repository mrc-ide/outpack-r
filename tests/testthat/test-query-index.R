test_that("index can be filtered for querying", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(tmp))

  index <- new_query_index(root, FALSE)
  expect_setequal(index$index$id, ids)
  expect_equal(index$index, index$get_index_unfiltered()$index)

  index$filter(ids[[1]])
  expect_equal(index$index$id, ids[[1]])
  expect_setequal(index$get_index_unfiltered()$index$id, ids)
})


test_that("index can include only unpacked packets", {
  t <- temp_file()
  path <- list()
  path$src <- file.path(t, "src")
  path$dst <- file.path(t, "dst")
  root <- list()
  root$src <- outpack_init(path$src)
  root$dst <- outpack_init(path$dst)
  outpack_location_add("src", "path", list(path = path$src),
                       root = path$dst)

  x1 <- create_random_packet(path$src, "x")
  x2 <- create_random_packet(path$src, "x")
  outpack_location_pull_metadata(root = path$dst)

  index <- new_query_index(path$dst, FALSE)
  index_unpacked <- new_query_index(path$dst, TRUE)
  expect_setequal(index$index$id, c(x1, x2))
  expect_equal(index_unpacked$index$id, character(0))

  for (i in c(x1, x2)) {
    outpack_location_pull_packet(i, location = "src", root = path$dst)
  }

  index <- new_query_index(path$dst, FALSE)
  index_unpacked <- new_query_index(path$dst, TRUE)
  expect_setequal(index$index$id, c(x1, x2))
  expect_setequal(index_unpacked$index$id, c(x1, x2))
})


test_that("index can be scoped", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(tmp))

  index <- new_query_index(root, FALSE)
  expect_setequal(index$index$id, ids)
  expect_equal(index$index, index$get_index_scoped()$index)

  index$scope(ids[[1]])
  expect_equal(index$index$id, ids[[1]])
  expect_setequal(index$get_index_scoped()$index$id, ids[[1]])
})
