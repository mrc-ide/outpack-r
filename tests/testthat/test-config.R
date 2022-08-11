test_that("Validate inputs to config set", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)

  expect_error(
    outpack_config_set(TRUE, root = root),
    "'options' must be named")
  expect_error(
    outpack_config_set(a = 1, options = list(b = 2), root = root),
    "If 'options' is given, no dot arguments are allowed")
})


test_that("Setting no options does nothing", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  config <- root$config
  outpack_config_set(root = root)
  expect_identical(outpack_root_open(path)$config, config)
})


test_that("Disallow unknown configuration options", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_error(
    outpack_config_set(whatever = TRUE, root = root),
    "Can't set configuration option: 'whatever'")
})


test_that("Can update core.require_complete_tree in empty archive", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_false(root$config$core$require_complete_tree)

  outpack_config_set(core.require_complete_tree = TRUE, root = root)

  expect_true(root$config$core$require_complete_tree)
  expect_true(outpack_root_open(root$path)$config$core$require_complete_tree)
})


test_that("Can remove file_store if path_archive exists", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path, use_file_store = TRUE)
  expect_true(root$config$core$use_file_store)

  file_store <- root$files$path
  expect_true(fs::dir_exists(file_store))

  expect_message(outpack_config_set(core.use_file_store = TRUE, root = root),
                 "'core.use_file_store' was unchanged")
  expect_true(root$config$core$use_file_store)
  expect_true(fs::dir_exists(file_store))

  outpack_config_set(core.use_file_store = FALSE, root = root)

  expect_false(root$config$core$use_file_store)
  expect_false(outpack_root_open(root$path)$config$core$use_file_store)
  expect_false(fs::dir_exists(file_store))
})


test_that("Cannot remove file_store if no path_archive", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path, use_file_store = TRUE, path_archive = NULL)
  file_store <- root$files$path
  expect_true(fs::dir_exists(file_store))
  expect_error(outpack_config_set(core.use_file_store = FALSE, root = root),
               "if 'path_archive' is NULL, then 'use_file_store' must be TRUE")

  expect_true(root$config$core$use_file_store)
  expect_true(outpack_root_open(root$path)$config$core$use_file_store)
  expect_true(fs::dir_exists(file_store))
})


test_that("Can add file_store", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  id1 <- create_random_packet(root, name = "example1")
  id2 <- create_random_packet(root, name = "example2")
  outpack_config_set(core.use_file_store = TRUE, root = root)
  expect_true(root$config$core$use_file_store)
  expect_true(outpack_root_open(root$path)$config$core$use_file_store)
  expect_true(fs::dir_exists(root$files$path))

  hash1 <- root$metadata(id1)$files$hash
  expect_true(length(hash1) == 1)
  hash2 <- root$metadata(id2)$files$hash
  expect_true(length(hash2) == 1)

  dest <- tempdir()
  on.exit(unlink(dest, recursive = TRUE))
  root$files$get(hash1, dest)
  root$files$get(hash2, dest)
})


test_that("Enabling recursive pulls forces pulling missing packets", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  root$src <- outpack_init(file.path(path, "src"))
  root$dst <- outpack_init(file.path(path, "dst"))
  expect_false(root$dst$config$core$require_complete_tree)

  id <- create_random_packet_chain(root$src, 3)
  outpack_location_add("src", root$src$path, root = root$dst$path)
  outpack_location_pull_metadata(root = root$dst$path)
  outpack_location_pull_packet(id[["c"]], root = root$dst$path)
  expect_equal(root$dst$index()$unpacked$packet, id[["c"]])

  outpack_config_set(core.require_complete_tree = TRUE, root = root$dst$path)

  expect_setequal(root$dst$index()$unpacked$packet, id)
  expect_true(
    outpack_root_open(root$dst$path)$config$core$require_complete_tree)
})


test_that("Unchanged require_complete_tree prints message", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_false(root$config$core$require_complete_tree)
  expect_message(
    outpack_config_set(core.require_complete_tree = FALSE, root = root),
    "'core.require_complete_tree' was unchanged")
  expect_silent(
    outpack_config_set(core.require_complete_tree = TRUE, root = root))
  expect_message(
    outpack_config_set(core.require_complete_tree = TRUE, root = root),
    "'core.require_complete_tree' was unchanged")
})
