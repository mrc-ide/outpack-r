test_that("can copy files from outpack", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  id <- create_random_packet(root)
  dst <- temp_file()
  outpack_copy_files(id, c("incoming.rds" = "data.rds"), dst, root)
  expect_equal(dir(dst), "incoming.rds")
  expect_identical(
    readRDS(file.path(dst, "incoming.rds")),
    readRDS(file.path(root$path, "archive", "data", id, "data.rds")))
})
