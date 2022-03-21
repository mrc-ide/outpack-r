test_that("can construct a outpack_location_path object", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  outpack_init(path)
  loc <- outpack_location_path$new(path)
  expect_s3_class(loc, "outpack_location_path")
  dat <- loc$list()
  expect_equal(nrow(dat), 0)
  expect_s3_class(dat, "data.frame")
  expect_equal(names(dat), c("packet", "time", "hash"))
})


test_that("outpack_location_path requires exact root", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  expect_error(
    outpack_location_path$new(path),
    "File does not exist:")

  outpack_init(path)
  subdir <- file.path(path, "subdir")
  dir.create(subdir)
  expect_error(
    outpack_location_path$new(subdir),
    "'.+subdir' does not look like an outpack root")

  expect_silent(outpack_location_path$new(path))
})


test_that("outpack_location_path returns list of packet ids", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  outpack_init(path)
  loc <- outpack_location_path$new(path)

  ids <- vcapply(1:3, function(i) create_random_packet(path))

  dat <- loc$list()
  expect_s3_class(dat, "data.frame")
  expect_equal(dat$packet, ids)
  expect_s3_class(dat$time, "POSIXt")
  str <- vcapply(file.path(path, ".outpack", "metadata", ids), read_string)
  expect_equal(
    dat$hash,
    vcapply(str, hash_data, "sha256", USE.NAMES = FALSE))
})


test_that("outpack_location_path can return metadata", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  outpack_init(path)
  loc <- outpack_location_path$new(path)

  ids <- vcapply(1:3, function(i) create_random_packet(path))
  str <- setNames(
    vcapply(file.path(path, ".outpack", "metadata", ids), read_string),
    ids)

  expect_equal(loc$metadata(ids[[2]]), str[2])
  expect_equal(loc$metadata(ids), str)
  expect_equal(loc$metadata(rep(ids[[1]], 2)), str[c(1, 1)])
})


test_that("requesting nonexistant metadata is an error", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  outpack_init(path)
  loc <- outpack_location_path$new(path)
  ids <- vcapply(1:3, function(i) create_random_packet(path))

  errs <- c("20220317-125935-ee5fd50e", "20220317-130038-48ffb8ba")
  expect_error(loc$metadata(errs[[1]]),
               "Some packet ids not found: '20220317-125935-ee5fd50e'")
  ## Truncating this error, will comma-separate all ids
  expect_error(
    loc$metadata(errs),
    "Some packet ids not found: '20220317-125935-ee5fd50e', '20220317")

  expect_error(loc$metadata(c(ids[[1]], errs[[1]], ids[[2]])),
               "Some packet ids not found: '20220317-125935-ee5fd50e'")
})
