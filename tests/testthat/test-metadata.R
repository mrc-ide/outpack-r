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
})
