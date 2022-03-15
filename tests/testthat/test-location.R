test_that("Can't add a location with reserved name", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)

  path_upstream <- tempfile()
  on.exit(unlink(path_upstream, recursive = TRUE), add = TRUE)
  upstream <- outpack_init(path_upstream)

  expect_error(
    outpack_location_add("local", path_upstream, path),
    "Cannot add a location with reserved name 'local'")
})


test_that("Can't add a location with existing name", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("a", "b", "c")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p))
  }

  outpack_location_add("upstream", root$b$path, root = root$a)
  expect_error(
    outpack_location_add("upstream", root$c$path, root = root$a),
    "A location with name 'upstream' already exists")
  expect_equal(outpack_location_list(root = root$a),
               c("local", "upstream"))
})
