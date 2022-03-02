test_that("can create new root", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  r <- outpack_init(path)
  expect_s3_class(r, "outpack_root")

  expect_true(file.exists(file.path(path, ".outpack", "metadata")))
  expect_true(file.exists(file.path(path, ".outpack", "location")))
})


test_that("Re-initialising root prints message", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  expect_silent(outpack_init(path))
  expect_message(r <- outpack_init(path),
                 "outpack already initialised at")
  expect_s3_class(r, "outpack_root")
})


test_that("Error if root cannot be found", {

})
