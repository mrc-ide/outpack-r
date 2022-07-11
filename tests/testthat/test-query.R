test_that("Can run very basic queries", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  root <- outpack_init(tmp, use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(tmp))
  expect_equal(
    outpack_query(quote(name == "data"), root = root),
    ids)
  expect_equal(
    outpack_query(quote(latest(name == "data")), root = root),
    last(ids))
  expect_equal(
    outpack_query(quote(name == "other"), root = root),
    character(0))
  expect_equal(
    outpack_query(quote(latest(name == "other")), root = root),
    NA_character_)
  expect_equal(
    outpack_query(quote(name == "other" || name == "data"), root = root),
    ids)
  expect_equal(
    outpack_query(quote(name == "other" && name == "data"), root = root),
    character(0))
  expect_equal(
    outpack_query(quote(!(name == "other") && name == "data"), root = root),
    ids)
})
