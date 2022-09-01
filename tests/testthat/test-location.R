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

  outpack_location_add("b", root$b$path, root = root$a)
  expect_setequal(outpack_location_list(root = root$a), c("local", "b"))

  outpack_location_add("c", root$c$path, root = root$a)
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
    outpack_location_add("local", path_upstream, root = path),
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
    "Directory does not exist:")
  fs::dir_create(other)
  expect_error(
    outpack_location_add("other", other, root = path),
    "'.+' does not look like an outpack root")
})


test_that("Can rename a location", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("a", "b")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p))
  }

  outpack_location_add("b", root$b$path, root = root$a)
  expect_setequal(outpack_location_list(root = root$a), c("local", "b"))

  outpack_location_rename("b", "c", root = root$a)
  expect_setequal(outpack_location_list(root = root$a), c("local", "c"))
})


test_that("Can't rename a location using an existent name", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("a", "b", "c")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p))
  }

  outpack_location_add("b", root$b$path, root = root$a)
  outpack_location_add("c", root$c$path, root = root$a)

  expect_error(outpack_location_rename("b", "c", root$a),
               "A location with name 'c' already exists")
  expect_error(outpack_location_rename("b", "local", root$a),
               "A location with name 'local' already exists")
})


test_that("Can't rename a  non-existent location", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_equal(outpack_location_list(root = path), "local")

  expect_error(outpack_location_rename("a", "b", root),
               "No location with name 'a' exists")
})


test_that("Can't rename default locations", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- outpack_init(path)

  expect_error(outpack_location_rename("local", "desktop", root),
               "Cannot rename default location 'local'")
  expect_error(outpack_location_rename("orphan", "removed", root),
               "Cannot rename default location 'orphan'")
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
  root_downstream <- outpack_root_open(path_downstream)
  index <- root_downstream$index()
  expect_length(index$metadata, 3)
  expect_setequal(names(index$metadata), ids)
  expect_mapequal(index$metadata, root_upstream$index()$metadata)

  expect_s3_class(index$location, "data.frame")
  expect_setequal(index$location$packet, ids)
  expect_equal(index$location$location,
               rep(lookup_location_id("upstream", root_downstream), 3))
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

  location_id <- lookup_location_id(c("x", "y", "z"),
                                    outpack_root_open(path$a))

  outpack_location_pull_metadata(c("x", "y"), root = path$a)
  index <- outpack_root_open(path$a)$index()
  expect_setequal(names(index$metadata), c(ids$x, ids$y))
  expect_equal(index$location$location, rep(location_id[1:2], each = 3))
  expect_equal(index$metadata[ids$x],
               outpack_root_open(path$x)$index()$metadata)
  expect_equal(index$metadata[ids$y],
               outpack_root_open(path$y)$index()$metadata)

  outpack_location_pull_metadata(root = path$a)
  index <- outpack_root_open(path$a)$index()
  expect_setequal(names(index$metadata), c(ids$x, ids$y, ids$z))
  expect_equal(index$location$location, rep(location_id, each = 3))
  expect_equal(index$metadata[ids$z],
               outpack_root_open(path$z)$index()$metadata)
})


test_that("Can't pull metadata from an unknown location", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  outpack_init(path)
  expect_error(
    outpack_location_pull_metadata("upstream", root = path),
    "Unknown location: 'upstream'")
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
  outpack_location_add("a", root$a$path, root = root$b)
  outpack_location_add("b", root$b$path, root = root$c)
  outpack_location_add("b", root$b$path, root = root$d)
  outpack_location_add("c", root$c$path, root = root$d)

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

  expect_equal(index$location$location,
               lookup_location_id(c("b", "c", "c"), root$d))
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
  outpack_location_pull_packet(id, root = root$dst)

  index <- root$dst$index()
  expect_equal(index$unpacked$packet, id)
  expect_equal(index$unpacked$location, lookup_location_id("src", root$dst))
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
  outpack_location_pull_packet(id, root = root$dst)

  index <- root$dst$index()
  expect_equal(index$unpacked$packet, id)
  expect_equal(index$unpacked$location, lookup_location_id("src", root$dst))
  expect_true(file.exists(
    file.path(root$dst$path, "archive", "data", id, "data.rds")))
})


test_that("detect and avoid modified files in source repository", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("src", "dst")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p), use_file_store = FALSE)
  }

  tmp <- fs::dir_create(tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  saveRDS(runif(10), file.path(tmp, "a.rds"))
  saveRDS(runif(10), file.path(tmp, "b.rds"))
  id <- character(2)
  for (i in seq_along(id)) {
    id[[i]] <- outpack_packet_start(tmp, "data", root = root$src)$id
    outpack_packet_end()
  }

  outpack_location_add("src", root$src$path, root = root$dst)
  outpack_location_pull_metadata(root = root$dst)

  ## Corrupt the file in the first id by truncating it:
  file.create(file.path(root$src$path, "archive", "data", id[[1]], "a.rds"))
  expect_message(
    outpack_location_pull_packet(id[[1]], "src", root = root$dst),
    sprintf("Rejecting file 'a.rds' in 'data/%s'", id[[1]]))

  expect_equal(
    hash_file(file.path(root$dst$path, "archive", "data", id[[1]], "a.rds")),
    hash_file(file.path(root$src$path, "archive", "data", id[[2]], "a.rds")))
  expect_equal(
    hash_file(file.path(root$dst$path, "archive", "data", id[[1]], "b.rds")),
    hash_file(file.path(root$src$path, "archive", "data", id[[2]], "b.rds")))
})


test_that("Do not unpack a packet twice", {
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

  expect_equal(
    outpack_location_pull_packet(id, "src", root = root$dst),
    character(0))
})


test_that("Sensible error if packet not known", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("src", "dst")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p), use_file_store = TRUE)
  }

  id <- create_random_packet(root$src)
  outpack_location_add("src", root$src$path, root = root$dst)
  expect_error(
    outpack_location_pull_packet(id, "src", root = root$dst),
    "Failed to find packet at location 'src': '.+'")
})


test_that("Can pull a tree recursively", {
  ## Bit of tedious setup here; this just does a simple graph
  ## >  a -> b -> c
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("src", "dst")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p), use_file_store = TRUE)
  }

  id <- list(a = create_random_packet(root$src, "a"))

  src_b <- file.path(path, "src_b")
  fs::dir_create(src_b)
  code <- "saveRDS(readRDS('input.rds') * 2, 'output.rds')"
  writeLines(code, file.path(src_b, "script.R"))
  id$b <- outpack_packet_start(src_b, "b", root = root$src)$id
  outpack_packet_use_dependency(id$a, c("input.rds" = "data.rds"))
  outpack_packet_run("script.R")
  outpack_packet_end()

  src_c <- file.path(path, "src_c")
  fs::dir_create(src_c)
  code <- "saveRDS(readRDS('input.rds') * 2, 'output.rds')"
  writeLines(code, file.path(src_c, "script.R"))
  id$c <- outpack_packet_start(src_c, "c", root = root$src)$id
  outpack_packet_use_dependency(id$b, c("input.rds" = "output.rds"))
  outpack_packet_run("script.R")
  outpack_packet_end()

  outpack_location_add("src", root$src$path, root = root$dst)
  outpack_location_pull_metadata(root = root$dst)
  expect_equal(
    outpack_location_pull_packet(id$c, "src", recursive = TRUE,
                                 root = root$dst),
    c(id$a, id$b, id$c))

  index <- root$dst$index()
  expect_equal(index$unpacked$packet,
               root$src$index()$unpacked$packet)
  expect_equal(index$unpacked$location,
               rep(lookup_location_id("src", root$dst), 3))

  expect_equal(
    outpack_location_pull_packet(id$c, "src", recursive = TRUE,
                                 root = root$dst),
    character(0))
})


test_that("Can add locations with different priorities", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("a", "b", "c")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p))
  }

  outpack_location_add("b", root$b$path, priority = 5, root = root$a)
  outpack_location_add("c", root$b$path, priority = 10, root = root$a)
  expect_equal(root$a$config$location$name, c("c", "b", "local"))
  expect_equal(root$a$config$location$priority, c(10, 5, 0))

  outpack_root_open(root$a$path)
  expect_equal(outpack_location_list(root$a),
               c("c", "b", "local"))
})


test_that("Can resolve locations", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("a", "b", "c", "d", "dst")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p))
  }

  priority <- c(a = -5, b = 20, c = 10, d = 15)
  for (i in names(priority)) {
    outpack_location_add(i, root[[i]]$path, priority = priority[[i]],
                         root = root$dst)
  }

  location_id <- set_names(
    lookup_location_id(c("a", "b", "c", "d", "local"), root$dst),
    c("a", "b", "c", "d", "local"))

  expect_equal(
    location_resolve_valid(NULL, root$dst, FALSE, FALSE),
    lookup_location_id(c("b", "d", "c", "a"), root$dst))
  expect_equal(
    location_resolve_valid(NULL, root$dst, TRUE, FALSE),
    lookup_location_id(c("b", "d", "c", "local", "a"), root$dst))
  expect_equal(
    location_resolve_valid(15, root$dst, FALSE, FALSE),
    lookup_location_id(c("b", "d"), root$dst))
  expect_equal(
    location_resolve_valid(0, root$dst, FALSE, FALSE),
    lookup_location_id(c("b", "d", "c"), root$dst))
  expect_equal(
    location_resolve_valid(0, root$dst, TRUE, FALSE),
    lookup_location_id(c("b", "d", "c", "local"), root$dst))
  expect_equal(
    location_resolve_valid(c("a", "b", "local", "d"), root$dst, FALSE, FALSE),
    lookup_location_id(c("a", "b", "d"), root$dst))
  expect_equal(
    location_resolve_valid(c("a", "b", "local", "d"), root$dst, TRUE, FALSE),
    lookup_location_id(c("a", "b", "local", "d"), root$dst))

  expect_error(
    location_resolve_valid(TRUE, root$dst, TRUE, FALSE),
    "Invalid input for 'location'; expected NULL, character or numeric")
  expect_error(
    location_resolve_valid(c(1, 2), root$dst, TRUE, FALSE),
    "If 'location' is numeric it must be a scalar (but was length 2)",
    fixed = TRUE)

  expect_error(
    location_resolve_valid(50, root$dst, TRUE, FALSE),
    "No locations found with priority of at least 50")
  expect_error(
    location_resolve_valid("other", root$dst, TRUE, FALSE),
    "Unknown location: 'other'")
  expect_error(
    location_resolve_valid(c("a", "b", "f", "g"), root$dst, TRUE, FALSE),
    "Unknown location: 'f', 'g'")
})


test_that("informative error message when no locations configured", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  root <- outpack_init(path)
  expect_equal(
    location_resolve_valid(NULL, root, FALSE, TRUE),
    character(0))
  expect_error(
    location_resolve_valid(NULL, root, FALSE, FALSE),
    "No suitable location found")
  expect_error(
    outpack_location_pull_packet(outpack_id(), root = root),
    "No suitable location found")
})


## The test setup here is hard to do because we don't yet support
## having location_path filtering metadata to the packets that it can
## actually provide.
test_that("Can filter locations", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("a", "b", "c", "d", "dst")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p))
  }

  ids_a <- vcapply(1:3, function(i) create_random_packet(root$a$path))
  outpack_location_add("a", root$a$path, root = root$b)
  outpack_location_pull_metadata(root = root$b)
  ids_b <- c(ids_a,
             vcapply(1:3, function(i) create_random_packet(root$b$path)))
  ids_c <- vcapply(1:3, function(i) create_random_packet(root$c$path))
  outpack_location_add("a", root$a$path, root = root$d)
  outpack_location_add("c", root$c$path, root = root$d)
  outpack_location_pull_metadata(root = root$d)
  ids_d <- c(ids_c,
             vcapply(1:3, function(i) create_random_packet(root$d$path)))

  priority <- c(a = 20, b = 15, c = 10, d = 5)
  for (i in names(priority)) {
    outpack_location_add(i, root[[i]]$path, priority = priority[[i]],
                         root = root$dst)
  }
  outpack_location_pull_metadata(root = root$dst)

  ids <- unique(c(ids_a, ids_b, ids_c, ids_d))
  expected <- function(ids, location_name) {
    data_frame(packet = ids,
               location_id = lookup_location_id(location_name, root$dst),
               location_name = location_name)
  }
  locs <- function(location) {
    location_resolve_valid(location, root$dst,
                           include_local = FALSE,
                           allow_no_locations = FALSE)
  }

  expect_equal(
    location_build_pull_plan(ids, locs(NULL), root = root$dst),
    expected(ids,
             c("a", "a", "a", "b", "b", "b", "c", "c", "c", "d", "d", "d")))
  ## Invert priority order:
  expect_equal(
    location_build_pull_plan(ids, locs(c("d", "c", "b", "a")), root = root$dst),
    expected(ids,
             c("d", "d", "d", "b", "b", "b", "d", "d", "d", "d", "d", "d")))
  ## Drop redundant locations
  expect_equal(
    location_build_pull_plan(ids, locs(c("b", "d")), root = root$dst),
    expected(ids,
             c("b", "b", "b", "b", "b", "b", "d", "d", "d", "d", "d", "d")))

  ## Some corner cases:
  expect_equal(
    location_build_pull_plan(ids_a[[1]], locs(NULL), root = root$dst),
    expected(ids_a[[1]], "a"))
  expect_equal(
    location_build_pull_plan(character(), locs(NULL), root = root$dst),
    expected(character(), character()))

  ## Failure to find things:
  err <- expect_error(
    location_build_pull_plan(ids, locs(c("a", "b", "c")), root = root$dst),
    "Failed to find packets at location 'a', 'b', 'c'")
  expect_error(
    location_build_pull_plan(ids, locs(10), root = root$dst),
    err$message, fixed = TRUE)
})


test_that("nonrecursive pulls are prevented by configuration", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  for (p in c("src", "dst")) {
    fs::dir_create(file.path(path, p))
    root[[p]] <- outpack_init(file.path(path, p), require_complete_tree = TRUE)
  }

  id <- create_random_packet_chain(root$src, 3)



  expect_error(
    outpack_location_pull_packet(id[["c"]], recursive = FALSE, root = root$dst),
    "'recursive' must be TRUE (or NULL) with your configuration",
    fixed = TRUE)
})


test_that("if recursive pulls are required, pulls are recursive by default", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  root <- list()
  root$src <- outpack_init(file.path(path, "src"))
  root$shallow <- outpack_init(file.path(path, "shallow"))
  root$deep <- outpack_init(file.path(path, "deep"),
                            require_complete_tree = TRUE)

  id <- create_random_packet_chain(root$src, 3)

  for (r in root[c("shallow", "deep")]) {
    outpack_location_add("src", root$src$path, root = r)
    outpack_location_pull_metadata(root = r)
  }

  outpack_location_pull_packet(id[["c"]], recursive = NULL, root = root$shallow)
  expect_equal(root$shallow$index()$unpacked$packet, id[["c"]])

  outpack_location_pull_packet(id[["c"]], recursive = NULL, root = root$deep)
  expect_setequal(root$deep$index()$unpacked$packet, id)
})
