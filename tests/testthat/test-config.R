test_that("Can update core.require_pull_recursive in empty archive", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_false(root$config$core$require_pull_recursive)

  outpack_config_set(core.require_pull_recursive = TRUE, root = root)

  expect_true(root$config$core$require_pull_recursive)
  expect_true(outpack_root_open(root$path)$config$core$require_pull_recursive)
})


test_that("Enabling recursive pulls forces pulling missing packets", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  root$src <- outpack_init(file.path(path, "src"))
  root$dst <- outpack_init(file.path(path, "dst"))
  expect_false(root$dst$config$core$require_pull_recursive)

  id <- create_random_packet_chain(root$src, 3)
  outpack_location_add("src", root$src$path, root = root$dst$path)
  outpack_location_pull_metadata(root = root$dst$path)
  outpack_location_pull_packet(id[["c"]], root = root$dst$path)
  expect_equal(root$dst$index()$unpacked$packet, id[["c"]])

  outpack_config_set(core.require_pull_recursive = TRUE, root = root$dst$path)

  expect_setequal(root$dst$index()$unpacked$packet, id)
  expect_true(
    outpack_root_open(root$dst$path)$config$core$require_pull_recursive)
})
