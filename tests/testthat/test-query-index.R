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

  expect_equal(index$get_packet_depends(ids["a"], 1),     character(0))
  expect_equal(index$get_packet_depends(ids["a"], Inf),    character(0))
  expect_setequal(index$get_packet_depends(ids["b"], 1),  ids["a"])
  expect_setequal(index$get_packet_depends(ids["b"], Inf), ids["a"])
  expect_setequal(index$get_packet_depends(ids["c"], 1),  ids["b"])
  expect_setequal(index$get_packet_depends(ids["c"], Inf), ids[c("a", "b")])
  expect_setequal(index$get_packet_depends(ids["d"], 1),  ids[c("b", "c")])
  expect_setequal(index$get_packet_depends(ids["d"], Inf),
                  ids[c("a", "b", "c")])
  ## There is no double counting of dependencies
  expect_length(index$get_packet_depends(ids["d"], Inf), 3)
})
