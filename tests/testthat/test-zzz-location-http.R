describe("server integration tests", {
  path <- tempfile()
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
    dest <- tempfile()
    on.exit(unlink(dest), add = TRUE)
    res <- client_http$fetch_file(hash, dest)
    expect_identical(res, dest)
    expect_identical(hash_file(dest), hash)
  })

  it("throws compatible error on missing file", {
    path1 <- tempfile()
    path2 <- tempfile()
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
