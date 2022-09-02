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

  p <- file.path(tmp, "a")
  h <- hash_file(p)
  expect_equal(obj$put(p, h), h)
  expect_equal(obj$list(), h)
  expect_true(file.exists(obj$filename(h)))
  dest <- tempfile()
  expect_equal(obj$get(h, dest), dest)
  expect_true(file.exists(dest))
  expect_equal(hash_file(dest), h)

  for (i in letters[2:10]) {
    h <- hash_file(file.path(tmp, i))
    obj$put(file.path(tmp, i), h)
  }
  expect_length(obj$list(), 10)
  expect_equal(file.exists(obj$filename(obj$list())),
               rep(TRUE, 10))
  dest <- tempfile()
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE))
  obj$get(obj$list(), dest)
})


test_that("can move files into place", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  for (i in 1:2) {
    saveRDS(runif(10), file.path(tmp, letters[i]))
  }
  pa <- file.path(tmp, "a")
  pb <- file.path(tmp, "b")
  ha <- hash_file(pa)
  hb <- hash_file(pb)

  obj <- file_store$new(tempfile())
  on.exit(unlink(obj$path, recursive = TRUE), add = TRUE)
  obj$put(pa, ha, move = FALSE)
  ## Original file has not been moved:
  expect_true(file.exists(pa))

  ## Moving files removes the source file whether or not a move was
  ## completed:
  obj$put(pa, ha, move = TRUE)
  obj$put(pb, hb, move = TRUE)
  expect_false(file.exists(pa))
  expect_false(file.exists(pb))
})


test_that("can create a filename within the store", {
  obj <- file_store$new(tempfile())
  on.exit(unlink(obj$path, recursive = TRUE), add = TRUE)
  p <- obj$tmp()
  expect_equal(normalizePath(dirname(p)),
               normalizePath(file.path(obj$path, "tmp")))
  expect_false(file.exists(p))
  expect_true(file.exists(file.path(obj$path, "tmp")))
})
