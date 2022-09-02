test_that("can run server", {
  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  server <- outpack_server(path)

  client <- outpack_location_http$new(server$url(""))
  cmp <- outpack_location_path$new(path)

  expect_identical(client$list(),
                   cmp$list())

  ids <- create_random_packet_chain(path, 4)

  ## There is a small issue in that the server is incorrectly
  ## serialising the time; save it as fractional seconds since the
  ## epoch (the same as the serialised format) so that this then
  ## works. Be sure to keep the full acuracy though.
  ## expect_identical(client$list(),
  ##                  cmp$list())

  expect_identical(client$metadata(ids),
                   cmp$metadata(ids))

  hash <- root$files$list()[[1]]
  dest <- tempfile()
  on.exit(unlink(dest), add = TRUE)
  res <- client$fetch_file(hash, dest)
  expect_identical(res, dest)
  expect_identical(hash_file(dest), hash)
})
