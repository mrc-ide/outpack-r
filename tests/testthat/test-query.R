test_that("Parse basic query", {
  res <- query_parse("latest(name == 'data')")
  expect_identical(query_parse(quote(latest(name == "data"))), res)
  expect_equal(res$type, "latest")
  expect_length(res$args, 1)
  expect_equal(res$args[[1]]$type, "test")
  expect_equal(res$args[[1]]$name, "==")
  expect_length(res$args[[1]]$args, 2)
  expect_equal(res$args[[1]]$args[[1]], list(type = "lookup", name = "name"))
  expect_equal(res$args[[1]]$args[[2]], list(type = "literal", value = "data"))
})


test_that("at_location requires at least one string literal", {
  err <- expect_error(
    query_parse(quote(latest(at_location(1, 2)))),
    "All arguments to at_location() must be string literals",
    fixed = TRUE)
  expect_match(err$message, "- in     at_location(1, 2)", fixed = TRUE)
  expect_match(err$message, "- within latest(at_location(1, 2))", fixed = TRUE)
  expect_error(
    query_parse(quote(latest(at_location("a", 2)))),
    "All arguments to at_location() must be string literals",
    fixed = TRUE)
  expect_error(
    query_parse(quote(latest(at_location()))),
    "Invalid call to at_location(); expected at least 1 args but recieved 0",
    fixed = TRUE)

  res <- query_parse(quote(at_location("a", "b")))
  expect_equal(res,
               list(type = "at_location",
                    args = list(list(type = "literal", value = "a"),
                                list(type = "literal", value = "b"))))
})


test_that("Can run very basic queries", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  root <- outpack_init(tmp, use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(tmp))
  expect_equal(
    outpack_query(quote(latest), root = root),
    last(ids))
  expect_equal(
    outpack_query(quote(latest()), root = root),
    last(ids))
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


test_that("location based queries", {
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

  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i)
      create_random_packet(root[[name]], "data", list(p = i)))
  }
  outpack_location_pull_metadata(root = path$a)

  expect_equal(
    outpack_query(quote(at_location("x", "y")), root = path$a),
    c(ids$x, ids$y))

  ## This is most similar to the functionality of orderly's
  ##
  ## > use_draft == "newer"
  expect_equal(
    outpack_query(quote(parameter:p == 2), quote(at_location("x", "y")),
                  root = path$a),
    c(ids$x[2], ids$y[2]))
})
