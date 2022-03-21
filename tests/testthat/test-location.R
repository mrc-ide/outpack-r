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


test_that("can pull metadata from a file base location", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))

  path_upstream <- file.path(tmp, "upstream")
  root_upstream <- outpack_init(path_upstream, use_file_store = TRUE)

  ids <- vcapply(1:3, function(i) create_random_packet(path_upstream))

  path_downstream <- file.path(tmp, "downstream")
  outpack_init(path_downstream, use_file_store = TRUE)

  outpack_location_add("upstream", path_upstream, root = path_downstream)
  expect_equal(outpack_location_list(root = path_downstream),
               c("local", "upstream"))

  outpack_location_pull_metadata("upstream", root = path_downstream)

  ## Sensible tests here will be much easier to write once we have a
  ## decent query interface.
  index <- outpack_root_open(path_downstream)$index()
  expect_length(index$metadata, 3)
  expect_setequal(names(index$metadata), ids)
  expect_mapequal(index$metadata, root_upstream$index()$metadata)

  expect_s3_class(index$location, "data.frame")
  expect_setequal(index$location$packet, ids)
  expect_equal(index$location$location, rep("upstream", 3))
})


test_that("can pull empty metadata", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))

  path_upstream <- file.path(tmp, "upstream")
  root_upstream <- outpack_init(path_upstream, use_file_store = TRUE)

  path_downstream <- file.path(tmp, "downstream")
  outpack_init(path_downstream, use_file_store = TRUE)

  outpack_location_add("upstream", path_upstream, root = path_downstream)
  outpack_location_pull_metadata("upstream", root = path_downstream)

  index <- outpack_root_open(path_downstream)$index()
  expect_length(index$metadata, 0)
  ## This is what we need to improve, everywhere
  expect_s3_class(index$location, "data.frame")
})



test_that("pull metadata from subset of locations", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  path <- root <- list()
  path$a <- file.path(tmp, "a")
  outpack_init(path$a, use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    path[[name]] <- file.path(tmp, name)
    root[[name]] <- outpack_init(path[[name]], use_file_store = TRUE)
    outpack_location_add(name, path[[name]], root = path$a)
  }

  expect_equal(outpack_location_list(root = path$a),
               c("local", "x", "y", "z"))

  ## NOTE: This is a little slow (0.2s) with about half of that coming
  ## from the call to utils::sessionInfo which gets bogged down
  ## reading DESCRIPTION files from disk - we might be better off
  ## replacing that with something a bit simpler. Also seeing some
  ## bottlenecks coming potentially from fs (fs::dir_create - looks
  ## like a known bug)
  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) create_random_packet(root[[name]]))
  }

  outpack_location_pull_metadata(c("x", "y"), root = path$a)
  index <- outpack_root_open(path$a)$index()
  expect_setequal(names(index$metadata), c(ids$x, ids$y))
  expect_equal(index$location$location, rep(c("x", "y"), each = 3))
  expect_equal(index$metadata[ids$x],
               outpack_root_open(path$x)$index()$metadata)
  expect_equal(index$metadata[ids$y],
               outpack_root_open(path$y)$index()$metadata)

  outpack_location_pull_metadata(root = path$a)
  index <- outpack_root_open(path$a)$index()
  expect_setequal(names(index$metadata), c(ids$x, ids$y, ids$z))
  expect_equal(index$location$location, rep(c("x", "y", "z"), each = 3))
  expect_equal(index$metadata[ids$z],
               outpack_root_open(path$z)$index()$metadata)
})


test_that("Can't pull metadata from an unknown location", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  outpack_init(path)
  expect_error(
    outpack_location_pull_metadata("upstream", root = path),
    "Unknown location 'upstream'")
})


test_that("No-op to pull metadata from no locations", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  outpack_init(path)
  expect_silent(outpack_location_pull_metadata("local", root = path))
  expect_silent(outpack_location_pull_metadata(root = path))
})


test_that("Can pull metadata through chain of locations", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("a", "b", "c", "d")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p))
  }

  ## More interesting topology, with a chain of locations, but d also
  ## knowing directly about an earlier location
  ## > a -> b -> c -> d
  ## >      `--------/
  outpack_location_add("a", root$a$path, root$b)
  outpack_location_add("b", root$b$path, root$c)
  outpack_location_add("c", root$c$path, root$d)
  outpack_location_add("b", root$b$path, root$d)

  ## Create a packet and make sure it's in both b and c
  id1 <- create_random_packet(root$a)
  outpack_location_pull_metadata(root = root$b)
  outpack_location_pull_metadata(root = root$c)

  ## And another in just 'c'
  id2 <- create_random_packet(root$c)

  ## Then when we pull from d it will simultaneously learn about the
  ## packet from both locations:
  outpack_location_pull_metadata(root = root$d)
  index <- root$d$index()

  ## Metadata is correct
  expect_length(index$metadata, 2)
  expect_equal(names(index$metadata), c(id1, id2))
  expect_equal(index$metadata, root$c$index()$metadata)

  ## Location information contains both sources
  expect_equal(nrow(index$location), 3)
  expect_equal(index$location$packet, c(id1, id1, id2))
  expect_equal(index$location$location, c("b", "c", "c"))
})


test_that("can pull a packet from one location to another, using file store", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("src", "dst")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p), use_file_store = TRUE)
  }

  id <- create_random_packet(root$src)
  outpack_location_add("src", root$src$path, root = root$dst)
  outpack_location_pull_metadata(root = root$dst)
  outpack_location_pull_packet(id, "src", root = root$dst)

  index <- root$dst$index()
  expect_equal(index$unpacked$packet, id)
  expect_equal(index$unpacked$location, "src")
  expect_true(file.exists(
    file.path(root$dst$path, "archive", "data", id, "data.rds")))
  expect_true(root$dst$files$exists(root$dst$metadata(id)$files$hash))
})


test_that("can pull a packet from one location to another, archive only", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("src", "dst")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p), use_file_store = FALSE)
  }

  id <- create_random_packet(root$src)
  outpack_location_add("src", root$src$path, root = root$dst)
  outpack_location_pull_metadata(root = root$dst)
  outpack_location_pull_packet(id, "src", root = root$dst)

  index <- root$dst$index()
  expect_equal(index$unpacked$packet, id)
  expect_equal(index$unpacked$location, "src")
  expect_true(file.exists(
    file.path(root$dst$path, "archive", "data", id, "data.rds")))
})
