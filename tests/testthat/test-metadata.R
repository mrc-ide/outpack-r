test_that("Can construct minimal metadata", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  time_end <- Sys.time()
  time <- list(start = time_end - 1, end = time_end)

  writeLines("some text", file.path(tmp, "script.R"))
  write.csv(mtcars, file.path(tmp, "data.csv"))

  json <- outpack_metadata_create(tmp, "tmp", "20220202-084931-35a26e55", time,
                                  inputs = "script.R", outputs = "data.csv")
  expect_s3_class(json, "json")

  expect_true(outpack_metadata_validate(json))

  dat <- outpack_metadata_load(json)
  expect_type(dat, "list")
  expect_setequal(
    names(dat),
    c("schemaVersion", "name", "id", "parameters", "time", "inputs",
      "outputs", "depends", "session"))

  path <- file.path(tmp, "metadata.json")
  writeLines(json, path)
  expect_identical(outpack_metadata_load(path), dat)
})


test_that("Can construct metadata with dependency", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  depends <- outpack_metadata_depends(
    "other",
    "20220202-084931-35a26e55",
    c("input.csv" = "output/data.csv"))

  time <- list(start = Sys.time(), end = Sys.time())

  writeLines("some text", file.path(tmp, "script.R"))
  write.csv(mtcars, file.path(tmp, "input.csv"))
  write.csv(mtcars * 2, file.path(tmp, "output.csv"))

  json <- outpack_metadata_create(tmp, "tmp", "20220202-084931-35a26e55", time,
                                  inputs = "script.R", depends = depends,
                                  outputs = "output.csv")
  expect_true(outpack_metadata_validate(json))

  expect_identical(
    outpack_metadata_create(tmp, "tmp", "20220202-084931-35a26e55", time,
                            inputs = "script.R", depends = list(depends),
                            outputs = "output.csv"),
    json)
})


test_that("validate dependencies", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  time <- list(start = Sys.time(), end = Sys.time())
  writeLines("some text", file.path(tmp, "script.R"))
  write.csv(mtcars, file.path(tmp, "input.csv"))
  write.csv(mtcars * 2, file.path(tmp, "output.csv"))
  expect_error(
    outpack_metadata_create(tmp, "tmp", "20220202-084931-35a26e55", time,
                            inputs = "script.R", depends = "input.csv",
                            outputs = "output.csv"),
    "'depends' must be a list")
  expect_error(
    outpack_metadata_create(tmp, "tmp", "20220202-084931-35a26e55", time,
                            inputs = "script.R", depends = list("input.csv"),
                            outputs = "output.csv"),
    "All elements of 'depends' must be 'outpack_metadata_depends'")
})


test_that("Can construct metadata with custom fields", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  time_end <- Sys.time()
  time <- list(start = time_end - 1, end = time_end)

  writeLines("some text", file.path(tmp, "script.R"))
  write.csv(mtcars, file.path(tmp, "data.csv"))

  extra <- list(a = 1:10,
                b = jsonlite::unbox(TRUE))

  json <- outpack_metadata_create(tmp, "tmp", "20220202-084931-35a26e55", time,
                                  inputs = "script.R", outputs = "data.csv",
                                  extra = extra)
  expect_true(outpack_metadata_validate(json))

  dat <- outpack_metadata_load(json)
  expect_equal(dat$a, as.list(1:10)) # joys of serialisation
  expect_equal(dat$b, TRUE)
})


test_that("Can construct metadata with parameters", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  time_end <- Sys.time()
  time <- list(start = time_end - 1, end = time_end)

  writeLines("some text", file.path(tmp, "script.R"))
  write.csv(mtcars, file.path(tmp, "data.csv"))

  parameters <- list(a = 1, b = 2)
  json <- outpack_metadata_create(tmp, "tmp", "20220202-084931-35a26e55", time,
                                  inputs = "script.R", outputs = "data.csv",
                                  parameters = parameters)
  expect_true(outpack_metadata_validate(json))

  dat <- outpack_metadata_load(json)
  expect_equal(dat$parameters, parameters)
})


test_that("Require at least one output", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  time <- list(start = Sys.time(), end = Sys.time())
  writeLines("some text", file.path(tmp, "script.R"))
  expect_error(
    outpack_metadata_create(tmp, "tmp", "20220202-084931-35a26e55", time,
                            inputs = "script.R", outputs = character()),
    "At least one 'outputs' is required")
})
