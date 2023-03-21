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


test_that("index includes depends info", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_dependent_packet(root, "d", ids[c("b", "c")])

  index <- new_query_index(root, FALSE)
  expect_setequal(index$index$id, ids)

  expect_equal(index$get_packet_depends(ids["a"], TRUE),     character(0))
  expect_equal(index$get_packet_depends(ids["a"], FALSE),    character(0))
  expect_setequal(index$get_packet_depends(ids["b"], TRUE),  ids["a"])
  expect_setequal(index$get_packet_depends(ids["b"], FALSE), ids["a"])
  expect_setequal(index$get_packet_depends(ids["c"], TRUE),  ids["b"])
  expect_setequal(index$get_packet_depends(ids["c"], FALSE), ids[c("a", "b")])
  expect_setequal(index$get_packet_depends(ids["d"], TRUE),  ids[c("b", "c")])
  expect_setequal(index$get_packet_depends(ids["d"], FALSE),
                  ids[c("a", "b", "c")])
  ## There is no double counting of dependencies
  expect_length(index$get_packet_depends(ids["d"], FALSE), 3)

  ## when we filter index
  index$filter(ids[c("b", "c")])

  ## then results from get_packet_depends are filtered too
  expect_equal(index$get_packet_depends(ids["a"], TRUE),     character(0))
  expect_equal(index$get_packet_depends(ids["a"], FALSE),    character(0))
  expect_setequal(index$get_packet_depends(ids["b"], TRUE),  character(0))
  expect_setequal(index$get_packet_depends(ids["b"], FALSE), character(0))
  expect_setequal(index$get_packet_depends(ids["c"], TRUE),  ids["b"])
  expect_setequal(index$get_packet_depends(ids["c"], FALSE), ids["b"])
  expect_setequal(index$get_packet_depends(ids["d"], TRUE),  ids[c("b", "c")])
  expect_setequal(index$get_packet_depends(ids["d"], FALSE), ids[c("b", "c")])
})
