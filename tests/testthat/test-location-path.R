test_that("can construct a outpack_location_path object", {
  root <- create_temporary_root()
  loc <- outpack_location_path$new(root$path)
  expect_s3_class(loc, "outpack_location_path")
  dat <- loc$list()
  expect_equal(nrow(dat), 0)
  expect_s3_class(dat, "data.frame")
  expect_equal(names(dat), c("packet", "time", "hash"))
})


test_that("outpack_location_path requires existing directory", {
  path <- temp_file()
  expect_error(
    outpack_location_path$new(path),
    "Directory does not exist:")
})


test_that("outpack_location_path requires exact root", {
  root <- create_temporary_root()
  subdir <- file.path(root$path, "subdir")
  dir.create(subdir)
  expect_error(
    outpack_location_path$new(subdir),
    "'.+subdir' does not look like an outpack root")

  expect_silent(outpack_location_path$new(root$path))
})


test_that("outpack_location_path returns list of packet ids", {
  root <- create_temporary_root()
  path <- root$path
  loc <- outpack_location_path$new(path)

  ids <- vcapply(1:3, function(i) create_random_packet(root$path))

  dat <- loc$list()
  expect_s3_class(dat, "data.frame")
  expect_equal(dat$packet, ids)
  expect_s3_class(dat$time, "POSIXt")
  str <- vcapply(file.path(path, ".outpack", "metadata", ids), read_string)
  expect_equal(
    dat$hash,
    vcapply(str, hash_data, "sha256", USE.NAMES = FALSE))
})


test_that("outpack_location_path can return metadata", {
  root <- create_temporary_root()
  path <- root$path
  loc <- outpack_location_path$new(path)

  ids <- vcapply(1:3, function(i) create_random_packet(path))
  str <- setNames(
    vcapply(file.path(path, ".outpack", "metadata", ids), read_string),
    ids)

  expect_equal(loc$metadata(ids[[2]]), str[2])
  expect_equal(loc$metadata(ids), str)
  expect_equal(loc$metadata(rep(ids[[1]], 2)), str[c(1, 1)])
})


test_that("requesting nonexistant metadata is an error", {
  root <- create_temporary_root()
  path <- root$path

  loc <- outpack_location_path$new(path)
  ids <- vcapply(1:3, function(i) create_random_packet(path))

  errs <- c("20220317-125935-ee5fd50e", "20220317-130038-48ffb8ba")
  expect_error(loc$metadata(errs[[1]]),
               "Some packet ids not found: '20220317-125935-ee5fd50e'")
  ## Truncating this error, will comma-separate all ids
  expect_error(
    loc$metadata(errs),
    "Some packet ids not found: '20220317-125935-ee5fd50e', '20220317")

  expect_error(loc$metadata(c(ids[[1]], errs[[1]], ids[[2]])),
               "Some packet ids not found: '20220317-125935-ee5fd50e'")
})


test_that("can locate files from the store", {
  root <- create_temporary_root(use_file_store = TRUE)
  path <- root$path

  loc <- outpack_location_path$new(path)
  ids <- vcapply(1:3, function(i) create_random_packet(path))
  idx <- root$index()

  files <- idx$metadata[[1]]$files
  h <- files$hash[files$path == "data.rds"]
  dest <- temp_file()
  res <- loc$fetch_file(h, dest)
  expect_identical(res, dest)
  expect_identical(hash_file(res), h)
})


test_that("sensible error if file not found in store", {
  root <- create_temporary_root(use_file_store = TRUE)
  path <- root$path

  loc <- outpack_location_path$new(path)
  h <- "md5:c7be9a2c3cd8f71210d9097e128da316"
  dest <- temp_file()
  expect_error(
    loc$fetch_file(h, dest),
    "Hash 'md5:c7be9a2c3cd8f71210d9097e128da316' not found at location")
  expect_false(file.exists(dest))
})


test_that("Can find file from archive", {
  root <- create_temporary_root(use_file_store = TRUE)
  path <- root$path

  loc <- outpack_location_path$new(path)
  ids <- vcapply(1:3, function(i) create_random_packet(path))
  idx <- root$index()

  files <- idx$metadata[[1]]$files
  h <- files$hash[files$path == "data.rds"]
  dest <- temp_file()
  res <- loc$fetch_file(h, dest)
  expect_identical(res, dest)
  expect_identical(hash_file(dest), h)
})


test_that("sensible error if file not found in archive", {
  root <- create_temporary_root(use_file_store = FALSE)
  path <- root$path

  loc <- outpack_location_path$new(path)
  h <- "md5:c7be9a2c3cd8f71210d9097e128da316"
  dest <- temp_file()
  expect_error(
    loc$fetch_file(h, dest),
    "Hash 'md5:c7be9a2c3cd8f71210d9097e128da316' not found at location")
  expect_false(file.exists(dest))
})


## This is the pattern that pete has; all comands are executed on the
## 'client' and the aim is to push the packets into the 'server':
test_that("Import complete tree via push into server with file store only", {
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)

  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  outpack_location_add("server", "path", list(path = server$path),
                       root = client)

  outpack_location_push(ids[[4]], "server", client)

  idx_c <- client$index()
  idx_s <- server$index()

  expect_equal(idx_s$metadata, idx_c$metadata)
  expect_equal(idx_s$unpacked$packet, idx_c$unpacked$packet)
  expect_equal(idx_s$location$packet, idx_c$location$packet)
  expect_equal(idx_s$location$hash, idx_c$location$hash)
})


test_that("Import complete tree via push into server with store and archive", {
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)

  server <- create_temporary_root(use_file_store = TRUE)
  outpack_location_add("server", "path", list(path = server$path),
                       root = client)

  outpack_location_push(ids[[4]], "server", client)

  idx_c <- client$index()
  idx_s <- server$index()

  expect_equal(idx_s$metadata, idx_c$metadata)
  expect_equal(idx_s$unpacked$packet, idx_c$unpacked$packet)
  expect_equal(idx_s$location$packet, idx_c$location$packet)
  expect_equal(idx_s$location$hash, idx_c$location$hash)

  files_c <- withr::with_dir(client$path,
                             fs::dir_ls("archive", all = TRUE, recurse = TRUE))
  files_s <- withr::with_dir(server$path,
                             fs::dir_ls("archive", all = TRUE, recurse = TRUE))
  expect_equal(files_c, files_s)

  i <- fs::is_file(file.path(client$path, files_c))
  expect_equal(
    hash_files(file.path(client$path, files_c[i])),
    hash_files(file.path(server$path, files_s[i])))
})


test_that("Import complete tree via push into server only archive", {
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)

  server <- create_temporary_root(use_file_store = FALSE)
  outpack_location_add("server", "path", list(path = server$path),
                       root = client)

  outpack_location_push(ids[[4]], "server", client)

  idx_c <- client$index()
  idx_s <- server$index()

  expect_equal(idx_s$metadata, idx_c$metadata)
  expect_equal(idx_s$unpacked$packet, idx_c$unpacked$packet)
  expect_equal(idx_s$location$packet, idx_c$location$packet)
  expect_equal(idx_s$location$hash, idx_c$location$hash)

  files_c <- withr::with_dir(client$path,
                             fs::dir_ls("archive", all = TRUE, recurse = TRUE))
  files_s <- withr::with_dir(server$path,
                             fs::dir_ls("archive", all = TRUE, recurse = TRUE))
  expect_equal(files_c, files_s)

  i <- fs::is_file(file.path(client$path, files_c))
  expect_equal(
    hash_files(file.path(client$path, files_c[i])),
    hash_files(file.path(server$path, files_s[i])))
})


## Here, we import a partial tree and check that it all behaves as
## expected.
test_that("Import partial tree via push into server with file store only", {
  client <- create_temporary_root()
  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  outpack_location_add("server", "path", list(path = server$path),
                       root = client)

  ## Create a packet on server
  id_base <- create_random_packet(server)

  ## Pull that into the client:
  outpack_location_pull_metadata(root = client)
  outpack_location_pull_packet(id_base, "server", root = client)

  ids <- create_random_packet_chain(client, 3, id_base)
  outpack_location_push(ids[[3]], "server", client)

  idx_c <- client$index()
  idx_s <- server$index()

  expect_mapequal(idx_s$metadata, idx_c$metadata)
  expect_setequal(idx_s$unpacked$packet, idx_c$unpacked$packet)
  expect_setequal(idx_s$location$packet, idx_c$location$packet)
  expect_setequal(idx_s$location$hash, idx_c$location$hash)
})
