test_that("Can run a basic packet", {
  ## TODO: We'll probably want to move some of these into test
  ## fixtures (or a demo directory like in orderly) as these will get
  ## boring to copy around. At the same time, it's pretty hard to
  ## remember which files will play well with each other, so keeping
  ## things explicit for now.

  ## A simple example where we run something.
  path_src <- tempfile()
  fs::dir_create(path_src)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src, "data.csv"),
            row.names = FALSE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)

  p <- outpack_packet_start(path_src, "example", root = root)

  outpack_packet_run("script.R")
  expect_true(file.exists(file.path(path_src, "myplot.png")))
  expect_equal(outpack_packet_current()$script, "script.R")

  outpack_packet_end()

  ## TODO: Still need a nice way of doing this:
  index <- root$index_update()
  expect_length(index$metadata, 1)
  id <- p$id

  path_metadata <- file.path(path, ".outpack", "metadata", id)
  expect_true(file.exists(path_metadata))
  outpack_schema("metadata")$validate(path_metadata)

  path_location <- file.path(path, ".outpack", "location", "local", id)
  expect_true(file.exists(path_location))
  outpack_schema("location")$validate(path_location)

  meta <- outpack_metadata_load(path_metadata)

  ## The index metadata is a subset of the full set:
  expect_mapequal(index$metadata[[id]],
                  meta[c("name", "id", "parameters", "files", "depends")])

  expect_equal(meta$schemaVersion, outpack_schema_version())
  expect_equal(meta$name, "example")
  expect_equal(meta$id, id)
  expect_null(meta$parameters)
  expect_equal(meta$depends, data_frame(id = character(), files = I(list())))
  expect_setequal(meta$files$path, c("data.csv", "myplot.png", "script.R"))
  expect_equal(meta$files$size,
               file.size(file.path(path_src, meta$files$path)))
  expect_equal(meta$files$hash,
               hash_files(file.path(path_src, meta$files$path)))

  ## Copy of the files in human readable archive:
  expect_true(all(file.exists(
    file.path(path, "archive", "example", id, meta$files$path))))
  expect_equal(
    hash_files(file.path(path, "archive", "example", id, meta$files$path)),
    meta$files$hash)

  ## Copy of the files in the file store:
  expect_setequal(root$files$list(), meta$files$hash)

  ## Easily retrieve metadata from root:
  expect_equal(root$metadata(id), index$metadata[[id]])
})


test_that("Can handle dependencies", {
  ## A simple example where we run something.
  path_src1 <- tempfile()
  fs::dir_create(path_src1)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src1, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src1, "data.csv"),
            row.names = FALSE)

  path_src2 <- tempfile()
  fs::dir_create(path_src2)
  writeLines(c(
    "d <- read.csv('incoming.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src2, "script.R"))

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  id1 <- p1$id
  outpack_packet_run("script.R")
  outpack_packet_end()

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  id2 <- p2$id
  outpack_packet_use_dependency(id1, c("incoming.csv" = "data.csv"))
  outpack_packet_run("script.R")
  outpack_packet_end()

  meta <- outpack_root_open(path)$metadata(id2)
  expect_equal(
    meta$depends,
    data_frame(
      id = id1,
      files = I(list(data_frame(path = "incoming.csv", source = "data.csv")))))
})
