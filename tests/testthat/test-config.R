test_that("Can update core.require_pull_recursive in empty archive", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_false(root$config$core$require_pull_recursive)

  outpack_config_set(core.require_pull_recursive = TRUE, root = root)

  expect_true(root$config$core$require_pull_recursive)
  expect_true(outpack_root_open(root$path)$config$core$require_pull_recursive)
})
