test_that("Parse basic query", {
  res <- query_parse("latest(name == 'data')", NULL, emptyenv())
  expect_identical(query_parse(quote(latest(name == "data")), NULL, emptyenv()),
                   res)
  expect_equal(res$type, "latest")
  expect_length(res$args, 1)
  expect_equal(res$args[[1]]$type, "test")
  expect_equal(res$args[[1]]$name, "==")
  expect_length(res$args[[1]]$args, 2)
  expect_equal(res$args[[1]]$args[[1]], list(type = "lookup", name = "name"))
  expect_equal(res$args[[1]]$args[[2]], list(type = "literal", value = "data"))
})


test_that("Prevent unparseable queries", {
  expect_error(query_parse(NULL, NULL, emptyenv()),
               "Invalid input for query")
  expect_error(query_parse("latest(); latest()", NULL, emptyenv()),
               "Expected a single expression")
})


test_that("print context around parse errors", {
  err <- expect_error(
    query_parse(quote(a %in% b), NULL, emptyenv()),
    "Invalid query 'a %in% b'; unknown query component '%in%'",
    fixed = TRUE)
  expect_match(err$message, "  - in a %in% b", fixed = TRUE)

  err <- expect_error(
    query_parse(quote(latest(a %in% b)), NULL, emptyenv()),
    "Invalid query 'a %in% b'; unknown query component '%in%'",
    fixed = TRUE)
  expect_match(err$message, "  - in     a %in% b", fixed = TRUE)
  expect_match(err$message, "  - within latest(a %in% b)", fixed = TRUE)
})


test_that("Expressions must be calls", {
  expect_error(
    query_parse(quote(name), NULL, emptyenv()),
    "Invalid query 'name'; expected some sort of expression")
  expect_error(
    query_parse(quote(latest(name)), NULL, emptyenv()),
    "Invalid query 'name'; expected some sort of expression")
  expect_error(
    query_parse(quote(latest(parameter:x == 1 && name)), NULL, emptyenv()),
    "Invalid query 'name'; expected some sort of expression")
})


test_that("validate argument numbers", {
  ## Have to do a fiddle here, to fail the arg length check. The error
  ## message is a bit weird too, but it will be reasonable for
  ## anything else that has a fixed number of args.
  expect_error(
    query_parse(quote(`==`(a, b, c)), NULL, emptyenv()),
    "Invalid call to ==(); expected 2 args but received 3",
    fixed = TRUE)
  expect_error(
    query_parse(quote(latest(a, b)), NULL, emptyenv()),
    "Invalid call to latest(); expected at most 1 args but received 2",
    fixed = TRUE)
  expect_error(
    query_parse(quote(latest(at_location())), NULL, emptyenv()),
    "Invalid call to at_location(); expected at least 1 args but received 0",
    fixed = TRUE)
})


test_that("at_location requires string literal arguments", {
  expect_error(
    query_parse(quote(latest(at_location(1, 2))), NULL, emptyenv()),
    "All arguments to at_location() must be string literals",
    fixed = TRUE)
  expect_error(
    query_parse(quote(latest(at_location("a", 2))), NULL, emptyenv()),
    "All arguments to at_location() must be string literals",
    fixed = TRUE)

  res <- query_parse(quote(at_location("a", "b")), NULL, emptyenv())
  expect_equal(res$type, "at_location")
  expect_equal(res$args, list(list(type = "literal", value = "a"),
                              list(type = "literal", value = "b")))
})


test_that("Queries can only be name and parameter", {
  res <- query_parse(quote(name == "data"), NULL, emptyenv())
  expect_equal(res$type, "test")
  expect_equal(res$name, "==")
  expect_equal(res$args,
               list(list(type = "lookup", name = "name"),
                    list(type = "literal", value = "data")))

  res <- query_parse(quote(parameter:x == 1), NULL, emptyenv())
  expect_equal(res$type, "test")
  expect_equal(res$name, "==")
  expect_equal(res$args,
               list(list(type = "lookup", name = "parameter", query = "x",
                         expr = quote(parameter:x),
                         context = quote(parameter:x == 1)),
                    list(type = "literal", value = 1)))
  expect_error(
    query_parse(quote(date >= "2022-02-04"), NULL, emptyenv()),
    "Unhandled query expression value 'date'")
  expect_error(
    query_parse(quote(custom:orderly:displayname >= "my name"), NULL,
                emptyenv()),
    "Invalid lookup 'custom:orderly'")
})


test_that("Can run very basic queries", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(tmp))
  expect_equal(
    outpack_query(quote(latest), root = root),
    last(ids))
  expect_equal(
    outpack_query(quote(latest()), root = root),
    last(ids))
  expect_equal(
    outpack_query(bquote(id == .(ids[[1]])), root = root),
    ids[[1]])
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
  expect_equal(
    outpack_query(bquote(latest(id == .(ids[[1]]) || id == .(ids[[2]]))),
      root = root),
    ids[[2]])
})


test_that("Scope queries", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- vcapply(1:3, function(i) create_random_packet(tmp, "x", list(a = 1)))
  x2 <- vcapply(1:3, function(i) create_random_packet(tmp, "x", list(a = 2)))
  y1 <- vcapply(1:3, function(i) create_random_packet(tmp, "y", list(a = 1)))
  y2 <- vcapply(1:3, function(i) create_random_packet(tmp, "y", list(a = 2)))

  expect_equal(
    outpack_query(quote(parameter:a == 1), root = root),
    c(x1, y1))
  expect_equal(
    outpack_query(quote(parameter:a == 1), scope = quote(name == "x"),
                  root = root),
    x1)
})


test_that("location based queries", {
  tmp <- temp_file()
  path <- root <- list()
  path$a <- file.path(tmp, "a")
  outpack_init(path$a, use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    path[[name]] <- file.path(tmp, name)
    root[[name]] <- outpack_init(path[[name]], use_file_store = TRUE)
    outpack_location_add(name, "path", list(path = path[[name]]),
                         root = path$a)
  }

  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) {
      create_random_packet(root[[name]], "data", list(p = i))
    })
  }
  outpack_location_pull_metadata(root = path$a)

  expect_equal(
    outpack_query(quote(at_location("x", "y")), root = path$a),
    c(ids$x, ids$y))

  ## This is most similar to the functionality of orderly's
  ##
  ## > use_draft == "newer"
  expect_equal(
    outpack_query(quote(parameter:p == 2),
                  scope = quote(at_location("x", "y")),
                  root = path$a),
    c(ids$x[2], ids$y[2]))
})


test_that("Can filter based on given values", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- vcapply(1:3, function(i) create_random_packet(tmp, "x", list(a = 1)))
  x2 <- vcapply(1:3, function(i) create_random_packet(tmp, "x", list(a = 2)))

  expect_equal(
    outpack_query(quote(latest(parameter:a == this:a)),
                  pars = list(a = 1), root = root),
    last(x1))
  expect_equal(
    outpack_query(quote(latest(parameter:a == this:a)),
                  pars = list(a = 2), root = root),
    last(x2))
  expect_equal(
    outpack_query(quote(latest(parameter:a == this:a)),
                  pars = list(a = 3), root = root),
    NA_character_)
  expect_error(
    outpack_query(quote(latest(parameter:a == this:x)),
                  pars = list(a = 3), root = root),
    paste0("Did not find 'x' within given pars ('a')\n",
           "  - while evaluating this:x\n",
           "  - within           latest(parameter:a == this:x)"),
    fixed = TRUE)
})


test_that("single requires exactly one packet", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  ids <- vcapply(1:3, function(i) create_random_packet(tmp, "x", list(a = i)))
  expect_equal(outpack_query(quote(single(parameter:a == 2)), root = root),
               ids[[2]])
  expect_error(outpack_query(quote(single(parameter:a >= 2)), root = root),
               "Query found 2 packets, but expected exactly one")
  expect_error(outpack_query(quote(single(parameter:a > 10)), root = root),
               "Query did not find any packets")
})


test_that("switch statements will prevent regressions", {
  skip_if_not_installed("mockery")
  mockery::stub(query_parse_expr, "query_parse_check_call",
                mockery::mock("other"))
  expr <- quote(some_function())
  expect_error(
    query_parse_expr(expr, expr),
    "Unhandled expression [outpack bug - please report]",
    fixed = TRUE)

  expect_error(
    query_eval(list(type = "other")),
    "Unhandled expression [outpack bug - please report]",
    fixed = TRUE)
  expect_error(
    query_eval_lookup(list(name = "custom:orderly:displayname")),
    "Unhandled lookup [outpack bug - please report]",
    fixed = TRUE)
  expect_error(
    query_eval_group(list(name = "operator")),
    "Unhandled operator [outpack bug - please report]",
    fixed = TRUE)
})


test_that("Can filter query to packets that are locally available (unpacked)", {
  tmp <- temp_file()
  path <- root <- list()
  path$a <- file.path(tmp, "a")
  outpack_init(path$a, use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    path[[name]] <- file.path(tmp, name)
    root[[name]] <- outpack_init(path[[name]], use_file_store = TRUE)
    outpack_location_add(name, "path", list(path = path[[name]]),
                         root = path$a)
  }

  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) {
      create_random_packet(root[[name]], "data", list(p = i))
    })
  }
  outpack_location_pull_metadata(root = path$a)

  expect_equal(
    outpack_query(quote(at_location("x", "y")), root = path$a),
    c(ids$x, ids$y))
  expect_equal(
    outpack_query(quote(at_location("x", "y")), require_unpacked = TRUE,
                  root = path$a),
    character())

  for (i in ids$x) {
    outpack_location_pull_packet(i, root = path$a)
  }

  expect_equal(
    outpack_query(quote(at_location("x", "y")), root = path$a),
    c(ids$x, ids$y))
  expect_equal(
    outpack_query(quote(at_location("x", "y")), require_unpacked = TRUE,
                  root = path$a),
    ids$x)
})


test_that("scope and require_unpacked can be used together to filter query", {
  t <- temp_file()
  path <- list()
  path$src <- file.path(t, "src")
  path$dst <- file.path(t, "dst")
  root <- list()
  root$src <- outpack_init(path$src)
  root$dst <- outpack_init(path$dst)
  outpack_location_add("src", "path", list(path = path$src),
                       root = path$dst)

  x1 <- create_random_packet(path$src, "x", list(p = 1))
  x2 <- create_random_packet(path$src, "x", list(p = 1))
  y1 <- create_random_packet(path$src, "y", list(p = 1))
  y2 <- create_random_packet(path$src, "y", list(p = 1))
  outpack_location_pull_metadata(root = path$dst)

  expect_equal(
    outpack_query(quote(latest(parameter:p == 1)), require_unpacked = FALSE,
                  scope = quote(name == "x"),
                  root = path$dst),
    x2)
  expect_equal(
    outpack_query(quote(latest(parameter:p == 1)), require_unpacked = TRUE,
                  scope = quote(name == "x"),
                  root = path$dst),
    NA_character_)

  for (i in c(x1, y1)) {
    outpack_location_pull_packet(i, location = "src", root = path$dst)
  }

  expect_equal(
    outpack_query(quote(latest(parameter:p == 1)), require_unpacked = FALSE,
                  scope = quote(name == "x"),
                  root = path$dst),
    x2)
  expect_equal(
    outpack_query(quote(latest(parameter:p == 1)), require_unpacked = TRUE,
                  scope = quote(name == "x"),
                  root = path$dst),
    x1)
})


test_that("Parse literal id query", {
  id <- "20220722-085951-148b7686"
  res <- query_parse(id, NULL, emptyenv())
  expect_identical(query_parse(bquote(single(id == .(id))), NULL, emptyenv()),
                   res)
  expect_equal(res$type, "single")
  expect_length(res$args, 1)
  expect_equal(res$args[[1]]$type, "test")
  expect_equal(res$args[[1]]$name, "==")
  expect_length(res$args[[1]]$args, 2)
  expect_equal(res$args[[1]]$args[[1]], list(type = "lookup", name = "id"))
  expect_equal(res$args[[1]]$args[[2]], list(type = "literal", value = id))
})


test_that("outpack_query allows ids", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(tmp))
  expect_identical(outpack_query(ids[[1]], root = root), ids[[1]])
  expect_identical(outpack_query(ids[[2]], root = root), ids[[2]])
  expect_error(
    outpack_query("20220722-085951-148b7686", root = root),
    "Query did not find any packets")
})


test_that("correct behaviour with empty queries", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  expect_equal(outpack_query("latest", root = root), NA_character_)
  expect_equal(outpack_query(quote(name == "data"), root = root),
               character(0))
})

test_that("named queries", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- create_random_packet(tmp, "x", list(a = 1))
  x2 <- create_random_packet(tmp, "x", list(a = 2))
  y1 <- create_random_packet(tmp, "y", list(a = 1))
  y2 <- create_random_packet(tmp, "y", list(a = 2))

  expect_equal(
    outpack_query(quote(latest()), name = "x", root = root),
    x2)
  expect_equal(
    outpack_query(quote(latest()), scope = quote(parameter:a == 1),
                  name = "x", root = root),
    x1)
})


test_that("outpack_query can include subqueries", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- create_random_packet(tmp, "x", list(a = 1))
  x2 <- create_random_packet(tmp, "x", list(a = 2))
  y1 <- create_random_packet(tmp, "y", list(a = 1))
  y2 <- create_random_packet(tmp, "y", list(a = 2))

  expect_equal(
    outpack_query(quote(latest({sub})),
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    x2)
  expect_equal(
    outpack_query(
      quote({sub}),
      subquery = list(sub = quote(latest(name == "x"))),
      root = root),
    x2)
})


test_that("outpack_query returns useful error when subquery name unknown", {
  tmp <- temp_file()
  root <- outpack_init(tmp)

  expect_error(
    outpack_query(quote(latest({sub})),
                  root = root),
    paste0("Cannot locate subquery named 'sub'. No subqueries provided.\n",
           "  - in     {sub}\n",
           "  - within latest({sub})"),
    fixed = TRUE)
  expect_error(
    outpack_query(quote(latest({subq})),
                  subquery = list(sub = quote(name == "x"),
                                  foo = quote(name == "y")),
                  root = root),
    paste0("Cannot locate subquery named 'subq'. ",
           "Available subqueries are 'foo', 'sub'.\n",
           "  - in     {subq}\n",
           "  - within latest({subq})"),
    fixed = TRUE)
})


test_that("outpack_query returns no results when subquery has no results", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- create_random_packet(tmp, "x", list(a = 1))

  ## subquery itself has no results
  expect_equal(outpack_query(quote(latest(name == "y")), root = root),
               NA_character_)

  expect_equal(
    outpack_query(quote(latest({sub})),
                  subquery = list(sub = quote(name == "y")),
                  root = root),
    NA_character_)
})


test_that("subqueries cannot be used in tests e.g. ==, <, >= etc.", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  expect_error(
    outpack_query(quote({sub} > 2),
                  subquery = list(sub = quote(parameter:a == 2)),
                  root = root),
    paste0("Unhandled query expression value '{sub}'\n",
           "  - in     {sub}\n",
           "  - within {sub} > 2"),
    fixed = TRUE)

  expect_error(
    outpack_query(quote(latest({sub}) > 2),
                  subquery = list(sub = quote(parameter:a == 2)),
                  root = root),
    paste0("Unhandled query expression value 'latest({sub})'\n",
           "  - in     latest({sub})\n",
           "  - within latest({sub}) > 2"),
    fixed = TRUE)

  expect_error(
    outpack_query(quote(latest({sub} == "hello")),
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    paste0("Unhandled query expression value '{sub}'\n",
           "  - in     {sub}\n",
           '  - within latest({sub} == "hello")'),
    fixed = TRUE)
})


test_that("subqueries can be used in groups e.g. &&, ||, (), etc.", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- create_random_packet(tmp, "x", list(a = 1))
  x2 <- create_random_packet(tmp, "x", list(a = 2))
  y1 <- create_random_packet(tmp, "y", list(a = 2))

  expect_setequal(
    outpack_query(quote({sub} || parameter:a == 2),
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    c(x1, x2, y1))

  expect_setequal(
    outpack_query(quote(!{sub}),
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    y1)

  expect_setequal(
    outpack_query(quote(parameter:a == 1 && {sub} || name == "y"),
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    c(x1, y1))
  expect_setequal(
    outpack_query(quote(parameter:a == 1 && ({sub} || name == "y")),
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    x1)
})


test_that("subqueries can be used within single", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- create_random_packet(tmp, "x", list(a = 1))
  x2 <- create_random_packet(tmp, "x", list(a = 2))
  y1 <- create_random_packet(tmp, "y", list(a = 2))

  expect_error(
    outpack_query(quote(single({sub})),
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    paste0("Query found 2 packets, but expected exactly one\n",
           "  - while evaluating single({sub})"),
    fixed = TRUE)

  expect_equal(
    outpack_query(quote(single({sub})),
                  subquery = list(sub = quote(name == "y")),
                  root = root),
    y1)
})


test_that("subqueries cannot be used within at_location", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  expect_error(
    outpack_query(quote(at_location({sub})),
                  subquery = list(sub = quote("x")),
                  root = root),
    paste0("All arguments to at_location() must be string literals\n",
           "  - in at_location({sub})"),
    fixed = TRUE)
})


test_that("outpack_query can include anonymous subqueries", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- create_random_packet(tmp, "x", list(a = 1))
  x2 <- create_random_packet(tmp, "x", list(a = 2))
  y1 <- create_random_packet(tmp, "y", list(a = 1))
  y2 <- create_random_packet(tmp, "y", list(a = 2))

  expect_equal(
    outpack_query(quote(latest({name == "x"})),
                  root = root),
    x2)
})


test_that("anonymous subquery is printed nicely when it errors", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)

  x1 <- create_random_packet(tmp, "x", list(a = 1))

  expect_error(
    outpack_query(quote(latest({ at_location() })),
                  root = root),
    paste0("Invalid call to at_location(); ",
           "expected at least 1 args but received 0\n",
           "  - in     at_location()\n",
           "  - within latest({at_location()})"),
    fixed = TRUE)
})
