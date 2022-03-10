test_that("can create new root", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  r <- outpack_init(path)
  expect_s3_class(r, "outpack_root")

  expect_true(file.exists(file.path(path, ".outpack", "metadata")))
  expect_true(file.exists(file.path(path, ".outpack", "location")))
  expect_mapequal(r$config$core,
                  list(path_archive = "archive",
                       use_file_store = FALSE,
                       hash_algorithm = "sha256"))
  expect_false(file.exists(file.path(path, ".outpack", "files")))
  expect_equal(r$location_list(), "local")
})


test_that("Re-initialising root errors", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  expect_silent(outpack_init(path))
  expect_error(r <- outpack_init(path),
                 "outpack already initialised at")
})


test_that("Can control root config on initialisation", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  r <- outpack_init(path, path_archive = NULL, use_file_store = TRUE)
  expect_mapequal(r$config$core,
                  list(path_archive = NULL,
                       use_file_store = TRUE,
                       hash_algorithm = "sha256"))
  expect_true(file.exists(file.path(path, ".outpack", "files")))
})


test_that("Must include some packet storage", {
  path <- tempfile()
  expect_error(
    outpack_init(path, path_archive = NULL, use_file_store = FALSE),
    "if 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  expect_false(file.exists(path))
})


test_that("Can locate an outpack root", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  r <- outpack_init(path)
  p <- file.path(path, "a", "b", "c")
  fs::dir_create(p)
  expect_equal(
    outpack_root_locate(p)$path,
    outpack_root_locate(path)$path)
  expect_equal(
    with_dir(p, outpack_root_locate(NULL)$path),
    outpack_root_locate(path)$path)
  expect_identical(
    outpack_root_locate(r), r)
})


test_that("outpack_root_locate errors if it reaches toplevel", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  fs::dir_create(path)
  expect_error(
    outpack_root_locate(path),
    "Did not find existing outpack root from directory '.+'")
})


test_that("outpack_root_open does not recurse", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  r <- outpack_init(path)
  expect_identical(outpack_root_open(r), r)
  expect_equal(outpack_root_open(path)$path, path)

  p <- file.path(path, "a", "b", "c")
  fs::dir_create(p)
  expect_error(
    outpack_root_open(p),
    "'.+/a/b/c' does not look like an outpack root")
})


test_that("root configuration matches schema", {
  skip_if_not_installed("jsonvalidate")
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  r <- outpack_init(path)
  path_config <- file.path(path, ".outpack", "config.json")
  expect_true(outpack_schema("config")$validate(path_config))
})


test_that("Can't get nonexistant metadata", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  r <- outpack_init(path, path_archive = NULL, use_file_store = TRUE)
  id <- outpack_id()
  expect_error(
    r$metadata(id),
    sprintf("id '%s' not found in index", id))
})
