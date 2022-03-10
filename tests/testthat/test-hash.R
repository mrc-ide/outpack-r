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
