test_that("Can create file store", {
  tmp <- tempfile()
  obj <- file_store$new(tmp)
  expect_equal(obj$list(), character(0))
  expect_error(
    obj$get("md5:abcde"),
    "Hash 'md5:abcde' not found in store")
})


test_that("Can store files", {
  tmp <- tempfile()
  dir.create(tmp)
  for (i in 1:10) {
    saveRDS(runif(10), file.path(tmp, letters[i]))
  }

  obj <- file_store$new(tempfile())

  h <- obj$put(file.path(tmp, "a"))
  expect_equal(hash_file(file.path(tmp, "a")), h)
  expect_equal(obj$list(), h)
  expect_true(file.exists(obj$filename(h)))
  dest <- tempfile()
  expect_equal(obj$get(h, dest), dest)
  expect_true(file.exists(dest))
  expect_equal(hash_file(dest), h)

  for (i in letters[2:10]) {
    obj$put(file.path(tmp, i))
  }
  expect_length(obj$list(), 10)
  expect_equal(file.exists(obj$filename(obj$list())),
               rep(TRUE, 10))
})
