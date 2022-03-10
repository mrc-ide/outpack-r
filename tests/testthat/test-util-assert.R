test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_silent(assert_character("a"))
  expect_error(assert_character(1), "must be character")
  expect_error(assert_character(TRUE), "must be character")
})


test_that("assert_logical", {
  expect_silent(assert_logical(TRUE))
  expect_error(assert_logical(1), "must be logical")
  expect_error(assert_logical("true"), "must be logical")
})


test_that("assert_named", {
  expect_error(assert_named(1), "must be named")
  expect_error(assert_named(setNames(1:2, c("a", "a")), TRUE),
               "must have unique names")
  expect_silent(assert_named(setNames(1:2, c("a", "a")), FALSE))
})


test_that("assert_is", {
  expect_error(assert_is("x", "foo"), "must be a foo")
  expect_silent(assert_is(structure("x", class = "foo"), "foo"))
})


test_that("assert_file_exists", {
  tmp <- normalizePath(tempdir(), mustWork = TRUE)
  path <- tempfile(tmpdir = tmp)
  expect_error(assert_file_exists(path), "File does not exist")
  file.create(path)
  expect_silent(assert_file_exists(path))
  expect_silent(assert_file_exists(basename(path), workdir = tmp))
})


test_that("assert_directory", {
  tmp <- tempfile()
  file.create(tmp)
  expect_silent(assert_directory(dirname(tmp)))
  expect_error(assert_directory(tmp), "Directory must be a directory")
})


test_that("assert_relative_path", {
  expect_error(assert_relative_path(getwd()),
               "'getwd()' must be relative path",
               fixed = TRUE)
  expect_silent(assert_relative_path("relpath"))

  expect_silent(
    assert_relative_path("../my/path"))
  expect_error(
    assert_relative_path("../my/path", TRUE),
    "must not contain '..' path components")
  expect_error(
    assert_relative_path("my/../../path", TRUE),
    "must not contain '..' path components")
})
