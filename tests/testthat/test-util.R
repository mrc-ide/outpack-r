test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("canonical case: single file", {
  ## NOTE: these tests may not be safe on solaris
  root <- tempfile()
  dir.create(root)
  path <- "a"
  path_uppper <- toupper(path)
  full <- file.path(root, path)

  dir.create(dirname(full), FALSE, TRUE)
  file.create(full)

  withr::with_dir(root, {
    expect_true(file_has_canonical_case(path))
    expect_equal(file_canonical_case(path), path)
    expect_true(file_exists(path))
    expect_true(file_exists(path, check_case = TRUE))

    expect_false(file_has_canonical_case(path_uppper))
    expect_equal(file_canonical_case(path_uppper), path)
  })

  expect_true(file_exists(path, check_case = FALSE, workdir = root))
  expect_true(file_exists(path, check_case = TRUE, workdir = root))

  expect_false(file_exists(path_uppper, check_case = TRUE, workdir = root))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }
  expect_true(file_exists(path_uppper, check_case = FALSE, workdir = root))
  v <- file_exists(path_uppper, check_case = TRUE, workdir = root,
                   force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, path_uppper))
})


test_that("canonical case: relative path", {
  root <- tempfile()
  dir.create(root)
  path <- file.path("a", "b", "c")
  path_upper <- toupper(path)
  full <- file.path(root, path)

  dir.create(dirname(full), FALSE, TRUE)
  file.create(full)

  withr::with_dir(root, {
    expect_true(file_has_canonical_case(path))
    expect_equal(file_canonical_case(path), path)
    expect_true(file_exists(path))
    expect_true(file_exists(path, check_case = TRUE))

    expect_false(file_has_canonical_case(path_upper))
    expect_equal(file_canonical_case(path_upper), path)
  })

  expect_true(file_exists(path, check_case = FALSE, workdir = root))
  expect_true(file_exists(path, check_case = TRUE, workdir = root))

  expect_false(file_exists(path_upper, check_case = TRUE, workdir = root))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }

  expect_true(file_exists(path_upper, check_case = FALSE, workdir = root))
  v <- file_exists(path_upper, check_case = TRUE, workdir = root,
                   force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, path_upper))
})


test_that("canonical case: absolute path", {
  path <- file.path(tempfile(), "a", "b", "c")
  dir.create(dirname(path), FALSE, TRUE)
  file.create(path)
  path <- normalizePath(path, "/")
  PATH <- toupper(path) # nolint
  if (is_windows()) {
    ## On windows, use upper case drive letters here:
    path <- paste0(toupper(substr(path, 1, 1)),
                   substr(path, 2, nchar(path)))
  }

  expect_true(file_has_canonical_case(path))
  expect_equal(file_canonical_case(path), path)
  expect_true(file_exists(path))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_has_canonical_case(PATH))
  expect_equal(file_canonical_case(PATH), path)

  expect_true(file_exists(path, check_case = FALSE))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_exists(PATH, check_case = TRUE))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }
  expect_true(file_exists(PATH, check_case = FALSE))

  v <- file_exists(PATH, check_case = TRUE, force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: path splitting", {
  expect_equal(file_split_base("a/b/c"),
               list(path = c("a", "b", "c"), base = ".", absolute = FALSE))
  expect_equal(file_split_base("/a/b/c"),
               list(path = c("a", "b", "c"), base = "/", absolute = TRUE))
  expect_equal(file_split_base("c:/a/b/c"),
               list(path = c("a", "b", "c"), base = "c:/", absolute = TRUE))
  expect_equal(file_split_base("C:/a/b/c"),
               list(path = c("a", "b", "c"), base = "C:/", absolute = TRUE))
})


test_that("canonical case ignores missing windows truncated elements", {
  ## There are issues with either mocking or system calls for
  ## canonical case checking on solaris, but as it is case-sensitive
  ## the tests are not important.
  skip_if_not_installed("mockery")
  mock_dir <- mockery::mock(c("aaa", "aax"),
                            c("bbb", "bbx"),
                            c("ccc", "ccx"),
                            cycle = TRUE)
  mockery::stub(file_canonical_case, "dir", mock_dir)

  expect_equal(file_canonical_case("aaa/bbb~1/ccc"),
               "aaa/bbb~1/ccc")
  expect_equal(file_canonical_case("aaa/BBB~1/ccc"),
               "aaa/BBB~1/ccc")
})
