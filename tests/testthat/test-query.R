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


test_that("Scope queries", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- vcapply(1:3, function(i)
    create_random_packet(tmp, "x", list(a = 1)))
  x2 <- vcapply(1:3, function(i)
    create_random_packet(tmp, "x", list(a = 2)))
  y1 <- vcapply(1:3, function(i)
    create_random_packet(tmp, "y", list(a = 1)))
  y2 <- vcapply(1:3, function(i)
    create_random_packet(tmp, "y", list(a = 2)))

  expect_equal(
    outpack_query(quote(parameter:a == 1), root = root),
    c(x1, y1))
  expect_equal(
    outpack_query(quote(parameter:a == 1), quote(name == "x"), root = root),
    x1)
})
