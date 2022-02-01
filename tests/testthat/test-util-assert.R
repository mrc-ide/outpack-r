test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_error(assert_character(1), "must be character")
  expect_error(assert_character(TRUE), "must be character")
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
  path <- tempfile(tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  expect_error(assert_file_exists(path), "File does not exist")
  writeLines(character(0), path)
  expect_silent(assert_file_exists(path))
})


test_that("assert_file_exists: error in case", {
  skip_if_not_installed("mockery")
  mockery::stub(assert_file_exists, "file_exists",
                structure(c(TRUE, FALSE, FALSE),
                          incorrect_case = c(FALSE, TRUE, FALSE),
                          correct_case = c("FOO" = "foo")))
  expect_error(assert_file_exists(c("bar", "FOO", "gaz")),
               "File does not exist: 'FOO' (should be 'foo'), 'gaz'",
               fixed = TRUE)
})
