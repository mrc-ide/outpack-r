test_that("provide some feedback on corrupt contents", {
  ## This set of tests really just checks a bunch of assertions that
  ## should never be seen if everything works - the user should not be
  ## able to create a system that fails in this way.

  ## First, build a zip file that we will unpack and break.
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)
  server <- create_temporary_root(use_file_store = FALSE)
  outpack_location_add("server", "path", list(path = server$path),
                       root = client)
  location_id <- lookup_location_id("server", client)
  plan <- location_build_push_plan(ids, location_id, client)
  zipfile <- withr::local_tempfile(fileext = ".zip")
  create_export_zip(plan, client, zipfile)

  tmp <- withr::local_tempdir()
  zip::unzip(zipfile, exdir = tmp)
  path <- file.path(tmp, "outpack")
  contents <- jsonlite::fromJSON(file.path(path, "contents.json"))
  import_dir_validate(path, contents, server)

  ## Files are used, but not included. This one might be triggerable
  ## if the user has deleted files off the server after requesting the
  ## zip?
  contents_err <- contents
  contents_err$files <- contents_err[-c(4, 6)]
  expect_error(
    import_dir_validate(path, contents_err, server),
    "Incorrect contents: required files missing from zip (outpack bug?)",
    fixed = TRUE)

  ## This one might happen if the user somehow has deleted a packet in
  ## the interim?
  contents_err <- contents
  contents_err$metadata <- contents_err$metadata[-2, ]
  expect_error(
    import_dir_validate(path, contents_err, server),
    "Incorrect contents: required packets missing from zip (outpack bug?)",
    fixed = TRUE)

  ## This one is *really* unlikely
  contents_err <- contents
  contents_err$metadata$hash[2:3] <- contents_err$metadata$hash[3:2]
  expect_error(
    import_dir_validate(path, contents_err, server),
    "Incorrect contents; hash of metadata does not agree (outpack bug?)",
    fixed = TRUE)
})
