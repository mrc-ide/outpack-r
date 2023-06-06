test_that("index can include only unpacked packets", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root()
  }
  outpack_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)

  x1 <- create_random_packet(root$src, "x")
  x2 <- create_random_packet(root$src, "x")
  outpack_location_pull_metadata(root = root$dst)

  index <- new_query_index(root$dst, FALSE, NULL)
  index_unpacked <- new_query_index(root$dst, TRUE, NULL)
  expect_setequal(index$index$id, c(x1, x2))
  expect_equal(index_unpacked$index$id, character(0))

  for (i in c(x1, x2)) {
    outpack_location_pull_packet(i, location = "src", root = root$dst)
  }

  index <- new_query_index(root$dst, FALSE, NULL)
  index_unpacked <- new_query_index(root$dst, TRUE, NULL)
  expect_setequal(index$index$id, c(x1, x2))
  expect_setequal(index_unpacked$index$id, c(x1, x2))
})


test_that("index includes depends info", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_dependent_packet(root, "d", ids[c("b", "c")])

  index <- new_query_index(root, FALSE, NULL)
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


test_that("index includes uses info", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_dependent_packet(root, "d", ids[c("b", "c")])

  index <- new_query_index(root, FALSE, NULL)
  expect_setequal(index$index$id, ids)

  expect_setequal(index$get_packet_uses(ids["a"], 1),    ids["b"])
  expect_setequal(index$get_packet_uses(ids["a"], Inf),  ids[c("b", "c", "d")])
  expect_setequal(index$get_packet_uses(ids["b"], 1),    ids[c("c", "d")])
  expect_setequal(index$get_packet_uses(ids["b"], Inf),  ids[c("c", "d")])
  expect_setequal(index$get_packet_uses(ids["c"], 1),    ids["d"])
  expect_setequal(index$get_packet_uses(ids["c"], Inf),  ids["d"])
  expect_equal(index$get_packet_uses(ids["d"], 1),       character(0))
  expect_equal(index$get_packet_uses(ids["d"], Inf),     character(0))
  ## There is no double counting of dependencies
  expect_length(index$get_packet_uses(ids["a"], Inf), 3)
})


test_that("can apply a location filter to index", {
  root <- list()
  root$a <- create_temporary_root(use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
    outpack_location_add(name, "path", list(path = root[[name]]$path),
                         root = root$a)
  }

  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) {
      create_random_packet(root[[name]], "data", list(p = i))
    })
  }
  outpack_location_pull_metadata(root = root$a)

  idx <- new_query_index(root$a, FALSE, NULL)
  expect_setequal(idx$index$id, unlist(ids, FALSE, FALSE))

  expect_setequal(new_query_index(root$a, FALSE, "x")$index$id,
                  unlist(ids$x, FALSE, FALSE))

  expect_setequal(new_query_index(root$a, FALSE, c("x", "z"))$index$id,
                  unlist(ids[c("x", "z")], FALSE, FALSE))
})
