describe("server integration tests", {
  path <- temp_file()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  server <- outpack_server(path)

  url <- server$url("")
  client_http <- outpack_location_http$new(url)
  client_path <- outpack_location_path$new(path)

  it("returns sensible list data when empty", {
    expect_identical(client_http$list(),
                     client_path$list())
  })

  ids <- create_random_packet_chain(path, 4)

  it("returns sensible list data when non-empty", {
    expect_identical(client_http$list(),
                     client_path$list())
  })

  it("returns compatible metadata", {
    expect_identical(client_http$metadata(ids),
                     client_path$metadata(ids))
  })

  it("throws compatible error on missing metadata", {
    msg <- "Some packet ids not found: 'id-missing'"
    expect_error(client_http$metadata("id-missing"), msg)
    expect_error(client_path$metadata("id-missing"), msg)
  })

  it("can fetch files", {
    hash <- root$files$list()[[1]]
    dest <- temp_file()
    res <- client_http$fetch_file(hash, dest)
    expect_identical(res, dest)
    expect_identical(hash_file(dest), hash)
  })

  it("throws compatible error on missing file", {
    path1 <- temp_file()
    path2 <- temp_file()
    msg <- "Hash 'hash:abc123' not found at location"
    err_http <- expect_error(
      client_http$fetch_file("hash:abc123", path1),
      msg)
    err_path <- expect_error(
      client_path$fetch_file("hash:abc123", path1),
      msg)
    expect_false(file.exists(path1))
    expect_false(file.exists(path2))
  })
})


describe("http location integration tests", {
  path <- temp_file()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  server <- outpack_server(path)
  url <- server$url("")

  ids <- vcapply(1:3, function(i) create_random_packet(path))

  it("can pull metadata", {
    tmp <- temp_file()
    path_downstream <- file.path(tmp, "downstream")
    outpack_init(path_downstream, use_file_store = TRUE)
    expect_null(names(outpack_root_open(path_downstream)$index()$metadata))
    outpack_location_add("upstream", "http", list(url = url),
                         root = path_downstream)
    expect_equal(outpack_location_list(root = path_downstream),
                 c("local", "upstream"))
    outpack_location_pull_metadata("upstream", root = path_downstream)

    root_downstream <- outpack_root_open(path_downstream)
    idx <- root_downstream$index()
    expect_equal(names(idx$metadata), ids)
  })

  it("can locate files from the store", {
    hash <- root$files$list()[[1]]
    dest <- temp_file()
    loc <- outpack_location_http$new(url)
    res <- loc$fetch_file(hash, dest)
    expect_identical(res, dest)
    expect_identical(hash_file(res), hash)
  })

  test_that("sensible error if file not found in store", {
    loc <- outpack_location_http$new(url)
    h <- "md5:c7be9a2c3cd8f71210d9097e128da316"
    dest <- temp_file()
    expect_error(
      loc$fetch_file(h, dest),
      "Hash 'md5:c7be9a2c3cd8f71210d9097e128da316' not found at location")
    expect_false(file.exists(dest))
  })
})