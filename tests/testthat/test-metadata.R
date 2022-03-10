test_that("Can construct metadata with parameters", {
  id <- outpack_id()
  name <- "example"
  time <- list(start = Sys.time() - 1, end = Sys.time())
  parameters <- list(a = 1, b = "two")
  path <- "."
  json <- outpack_metadata_create(path, name, id, time,
                                  parameters = parameters,
                                  files = character(),
                                  script = character(),
                                  depends = NULL,
                                  session = NULL)
  d <- outpack_metadata_load(json)
  expect_equal(d$parameters, parameters)
})


test_that("Validate parameters", {
  expect_error(
    validate_parameters(list(a = 1, a = 1)),
    "'parameters' must have unique names")
  expect_error(
    validate_parameters(list(1, 1)),
    "'parameters' must be named")
  expect_error(
    validate_parameters(list(a = 1, b = 2:3)),
    "All parameters must be scalar atomics: error for 'b'")
  expect_error(
    validate_parameters(list(a = new.env(), b = 2:3)),
    "All parameters must be scalar atomics: error for 'a', 'b'")
  expect_error(
    validate_parameters(list(a = new.env(), b = 2:3, c = NA)),
    "All parameters must be scalar atomics: error for 'a', 'b', 'c'")
})
