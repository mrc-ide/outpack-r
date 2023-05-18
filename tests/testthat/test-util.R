test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("Descend failure", {
  path <- temp_file()
  dir.create(path)
  expect_null(find_file_descend(".orderly_foobar", tempdir(), path))
  expect_null(find_file_descend(".orderly_foobar", "/", path))
  expect_null(find_file_descend(".orderly_foobar", "/", "/"))
})


test_that("can source script that fails with error, reporting traceback", {
  path <- withr::local_tempfile(fileext = ".R")
  code <- 'f <- function(x) {
  message("calling with x = ", x)
  if (x == 0) {
    stop("x became zero")
  } else {
    f(x - 1)
  }
}
f(5)'
  writeLines(code, path)
  envir <- new.env()
  res <- source_and_capture(path, envir, FALSE)

  expect_setequal(names(res),
                  c("success", "error", "traceback", "warnings", "output"))
  expect_false(res$success)
  expect_s3_class(res$error, "error")
  expect_equal(res$warnings, list())
  expect_type(res$traceback, "character")

  skip_on_cran() # depends on details
  output <- strsplit(code, "\n")[[1]]
  output <- paste(rep(c(">", "+", ">"), c(1, length(output) - 2, 1)), output)
  output <- c(output,
              sprintf("calling with x = %d", 5:0),
              "Error in f(x - 1) : x became zero")
  expect_equal(res$output, output)
})


test_that("can capture warnings", {
  path <- withr::local_tempfile(fileext = ".R")
  code <- 'f <- function(x) {
  message("calling with x = ", x)
  if (x == 0) {
    0
  } else {
    warning("x still too large")
    f(x - 1)
  }
}
f(5)'
  writeLines(code, path)
  envir <- new.env()
  res <- source_and_capture(path, envir, FALSE)
  expect_type(res$warnings, "list")
  expect_length(res$warnings, 5)
  expect_equal(conditionMessage(res$warnings[[1]]), "x still too large")
})
