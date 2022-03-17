test_that("No locations except local by default", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_equal(outpack_location_list(root = path), "local")
})


test_that("Can add a location", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("a", "b", "c")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p))
  }

  outpack_location_add("b", root$b$path, root$a)
  expect_setequal(outpack_location_list(root = root$a), c("local", "b"))

  outpack_location_add("c", root$c$path, root$a)
  expect_setequal(outpack_location_list(root = root$a), c("local", "b", "c"))
})


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


test_that("Require that (for now) locations must be paths", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_equal(outpack_location_list(root = path), "local")

  other <- tempfile()
  on.exit(unlink(other, recursive = TRUE), add = TRUE)
  expect_error(
    outpack_location_add("other", other, root = path),
    "File does not exist:")
  fs::dir_create(other)
  expect_error(
    outpack_location_add("other", other, root = path),
    "'.+' does not look like an outpack root")
})
