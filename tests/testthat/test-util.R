test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("Can hash file", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  writeLines("hello world", tmp)
  expected <- unname(tools::md5sum(tmp))
  expect_equal(hash_file(tmp, "md5"), paste0("md5:", expected))
  expect_equal(hash_parse(hash_file(tmp, "md5")),
               list(algorithm = "md5", value = expected))

  saveRDS(mtcars, tmp)
  expected <- unname(tools::md5sum(tmp))
  expect_equal(hash_file(tmp, "md5"), paste0("md5:", expected))
})
