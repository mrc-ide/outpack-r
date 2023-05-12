test_that("Can run very basic queries", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(root))
  expect_equal(
    outpack_search(quote(latest), root = root),
    last(ids))
  expect_equal(
    outpack_search(quote(latest()), root = root),
    last(ids))
  expect_equal(
    outpack_search(bquote(id == .(ids[[1]])), root = root),
    ids[[1]])
  expect_equal(
    outpack_search(quote(name == "data"), root = root),
    ids)
  expect_equal(
    outpack_search(quote(latest(name == "data")), root = root),
    last(ids))
  expect_equal(
    outpack_search(quote(name == "other"), root = root),
    character(0))
  expect_equal(
    outpack_search(quote(latest(name == "other")), root = root),
    NA_character_)
  expect_equal(
    outpack_search(quote(name == "other" || name == "data"), root = root),
    ids)
  expect_equal(
    outpack_search(quote(name == "other" && name == "data"), root = root),
    character(0))
  expect_equal(
    outpack_search(quote(!(name == "other") && name == "data"), root = root),
    ids)
  expect_equal(
    outpack_search(bquote(latest(id == .(ids[[1]]) || id == .(ids[[2]]))),
      root = root),
    ids[[2]])
})


test_that("Scope queries", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 1)))
  x2 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 2)))
  y1 <- vcapply(1:3, function(i) create_random_packet(root, "y", list(a = 1)))
  y2 <- vcapply(1:3, function(i) create_random_packet(root, "y", list(a = 2)))

  expect_equal(
    outpack_search(quote(parameter:a == 1), root = root),
    c(x1, y1))
  expect_equal(
    outpack_search(quote(parameter:a == 1), scope = quote(name == "x"),
                  root = root),
    x1)
})


test_that("location based queries", {
  root <- list()
  root$a <- create_temporary_root(use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
    outpack_location_add(name, "path", list(path = root[[name]]$path),
                         root = root$a)
  }

  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) {
      create_random_packet(root[[name]], "data", list(p = i))
    })
  }
  outpack_location_pull_metadata(root = root$a)

  expect_equal(
    outpack_search(quote(at_location("x", "y")), root = root$a),
    c(ids$x, ids$y))

  ## This is most similar to the functionality of orderly's
  ##
  ## > use_draft == "newer"
  expect_equal(
    outpack_search(quote(parameter:p == 2),
                  scope = quote(at_location("x", "y")),
                  root = root$a),
    c(ids$x[2], ids$y[2]))
})


test_that("Can filter based on given values", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 1)))
  x2 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 2)))

  expect_equal(
    outpack_search(quote(latest(parameter:a == this:a)),
                  pars = list(a = 1), root = root),
    last(x1))
  expect_equal(
    outpack_search(quote(latest(parameter:a == this:a)),
                  pars = list(a = 2), root = root),
    last(x2))
  expect_equal(
    outpack_search(quote(latest(parameter:a == this:a)),
                  pars = list(a = 3), root = root),
    NA_character_)
  expect_error(
    outpack_search(quote(latest(parameter:a == this:x)),
                  pars = list(a = 3), root = root),
    paste0("Did not find 'x' within given pars ('a')\n",
           "  - while evaluating this:x\n",
           "  - within           latest(parameter:a == this:x)"),
    fixed = TRUE)
})


test_that("single requires exactly one packet", {
  root <- create_temporary_root(use_file_store = TRUE)

  ids <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = i)))
  expect_equal(outpack_search(quote(single(parameter:a == 2)), root = root),
               ids[[2]])
  expect_error(outpack_search(quote(single(parameter:a >= 2)), root = root),
               "Query found 2 packets, but expected exactly one")
  expect_error(outpack_search(quote(single(parameter:a > 10)), root = root),
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
  root <- list()
  root$a <- create_temporary_root(use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
    outpack_location_add(name, "path", list(path = root[[name]]$path),
                         root = root$a)
  }

  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) {
      create_random_packet(root[[name]], "data", list(p = i))
    })
  }
  outpack_location_pull_metadata(root = root$a)

  expect_equal(
    outpack_search(quote(at_location("x", "y")), root = root$a),
    c(ids$x, ids$y))
  expect_equal(
    outpack_search(quote(at_location("x", "y")), require_unpacked = TRUE,
                  root = root$a),
    character())

  for (i in ids$x) {
    outpack_location_pull_packet(i, root = root$a)
  }

  expect_equal(
    outpack_search(quote(at_location("x", "y")), root = root$a),
    c(ids$x, ids$y))
  expect_equal(
    outpack_search(quote(at_location("x", "y")), require_unpacked = TRUE,
                  root = root$a),
    ids$x)
})


test_that("scope and require_unpacked can be used together to filter query", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
  }
  outpack_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)

  x1 <- create_random_packet(root$src, "x", list(p = 1))
  x2 <- create_random_packet(root$src, "x", list(p = 1))
  y1 <- create_random_packet(root$src, "y", list(p = 1))
  y2 <- create_random_packet(root$src, "y", list(p = 1))
  outpack_location_pull_metadata(root = root$dst)

  expect_equal(
    outpack_search(quote(latest(parameter:p == 1)), require_unpacked = FALSE,
                  scope = quote(name == "x"),
                  root = root$dst),
    x2)
  expect_equal(
    outpack_search(quote(latest(parameter:p == 1)), require_unpacked = TRUE,
                  scope = quote(name == "x"),
                  root = root$dst),
    NA_character_)

  for (i in c(x1, y1)) {
    outpack_location_pull_packet(i, location = "src", root = root$dst)
  }

  expect_equal(
    outpack_search(quote(latest(parameter:p == 1)), require_unpacked = FALSE,
                  scope = quote(name == "x"),
                  root = root$dst),
    x2)
  expect_equal(
    outpack_search(quote(latest(parameter:p == 1)), require_unpacked = TRUE,
                  scope = quote(name == "x"),
                  root = root$dst),
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


test_that("outpack_search allows ids", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(root))
  expect_identical(outpack_search(ids[[1]], root = root), ids[[1]])
  expect_identical(outpack_search(ids[[2]], root = root), ids[[2]])
  expect_error(
    outpack_search("20220722-085951-148b7686", root = root),
    "Query did not find any packets")
})


test_that("correct behaviour with empty queries", {
  root <- create_temporary_root(use_file_store = TRUE)
  expect_equal(outpack_search("latest", root = root), NA_character_)
  expect_equal(outpack_search(quote(name == "data"), root = root),
               character(0))
})

test_that("named queries", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 1))
  y2 <- create_random_packet(root, "y", list(a = 2))

  expect_equal(
    outpack_search(quote(latest()), name = "x", root = root),
    x2)
  expect_equal(
    outpack_search(quote(latest()), scope = quote(parameter:a == 1),
                  name = "x", root = root),
    x1)
})


test_that("outpack_search can include subqueries", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 1))
  y2 <- create_random_packet(root, "y", list(a = 2))

  expect_equal(
    outpack_search(quote(latest({sub})), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    x2)
  expect_equal(
    outpack_search(
      quote({sub}), # nolint
      subquery = list(sub = quote(latest(name == "x"))),
      root = root),
    x2)
})


test_that("outpack_search returns useful error when subquery name unknown", {
  root <- create_temporary_root()

  expect_error(
    outpack_search(quote(latest({sub})), # nolint
                  root = root),
    paste0("Cannot locate subquery named 'sub'. No named subqueries ",
           "provided.\n",
           "  - in     {sub}\n",
           "  - within latest({sub})"),
    fixed = TRUE)

  expect_error(
    outpack_search(quote(latest({subq})), # nolint
                  subquery = list(sub = quote(name == "x"),
                                  foo = quote(name == "y")),
                  root = root),
    paste0("Cannot locate subquery named 'subq'. ",
           "Available subqueries are 'foo', 'sub'.\n",
           "  - in     {subq}\n",
           "  - within latest({subq})"),
    fixed = TRUE)

  ## Anonymous subqueries are not included in list
  expect_error(
    outpack_search(quote(latest({name == "x"} && {sub})), # nolint
                  root = root),
    "Cannot locate subquery named 'sub'. No named subqueries provided.")
})


test_that("outpack_search returns no results when subquery has no results", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))

  ## subquery itself has no results
  expect_equal(outpack_search(quote(latest(name == "y")), root = root),
               NA_character_)

  expect_equal(
    outpack_search(quote(latest({sub})), # nolint
                  subquery = list(sub = quote(name == "y")),
                  root = root),
    NA_character_)
})


test_that("subqueries cannot be used in tests e.g. ==, <, >= etc.", {
  root <- create_temporary_root(use_file_store = TRUE)

  expect_error(
    outpack_search(quote({sub} > 2), # nolint
                  subquery = list(sub = quote(parameter:a == 2)),
                  root = root),
    paste0("Unhandled query expression value '{sub}'\n",
           "  - in     {sub}\n",
           "  - within {sub} > 2"),
    fixed = TRUE)

  expect_error(
    outpack_search(quote(latest({sub}) > 2), # nolint
                  subquery = list(sub = quote(parameter:a == 2)),
                  root = root),
    paste0("Unhandled query expression value 'latest({sub})'\n",
           "  - in     latest({sub})\n",
           "  - within latest({sub}) > 2"),
    fixed = TRUE)

  expect_error(
    outpack_search(quote(latest({sub} == "hello")), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    paste0("Unhandled query expression value '{sub}'\n",
           "  - in     {sub}\n",
           '  - within latest({sub} == "hello")'),
    fixed = TRUE)
})


test_that("subqueries can be used in groups e.g. &&, ||, (), etc.", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 2))

  expect_setequal(
    outpack_search(quote({sub} || parameter:a == 2), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    c(x1, x2, y1))

  expect_setequal(
    outpack_search(quote(!{sub}), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    y1)

  expect_setequal(
    outpack_search(quote(parameter:a == 1 && {sub} || name == "y"), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    c(x1, y1))
  expect_setequal(
    outpack_search(quote(parameter:a == 1 && ({sub} || name == "y")), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    x1)
})


test_that("subqueries can be used within single", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 2))

  expect_error(
    outpack_search(quote(single({sub})), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    paste0("Query found 2 packets, but expected exactly one\n",
           "  - while evaluating single({sub})"),
    fixed = TRUE)

  expect_equal(
    outpack_search(quote(single({sub})), # nolint
                  subquery = list(sub = quote(name == "y")),
                  root = root),
    y1)
})


test_that("subqueries cannot be used within at_location", {
  root <- create_temporary_root(use_file_store = TRUE)

  expect_error(
    outpack_search(quote(at_location({sub})), # nolint
                  subquery = list(sub = quote("latest")),
                  root = root),
    paste0("All arguments to at_location() must be string literals\n",
           "  - in at_location({sub})"),
    fixed = TRUE)
})


test_that("outpack_search can include anonymous subqueries", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 1))
  y2 <- create_random_packet(root, "y", list(a = 2))

  expect_equal(
    outpack_search(quote(latest({name == "x"})), # nolint
                  root = root),
    x2)
})


test_that("anonymous subquery is printed nicely when it errors", {
  root <- create_temporary_root()

  x1 <- create_random_packet(root, "x", list(a = 1))

  expect_error(
    outpack_search(quote(latest({ at_location() })), # nolint
                  root = root),
    paste0("Invalid call to at_location(); ",
           "expected at least 1 args but received 0\n",
           "  - in     at_location()\n",
           "  - within latest({at_location()})"),
    fixed = TRUE)
})


test_that("subqueries respect scope", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 1))
  y2 <- create_random_packet(root, "y", list(a = 2))

  expect_equal(
    outpack_search(quote({report_x} || parameter:a == 2), # nolint
                  subquery = list(report_x = quote(name == "x")),
                  scope = quote(name == "y"),
                  root = root),
    y2)
})


describe("outpack_search can search for packets usedby another", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_dependent_packet(root, "d", ids[c("b", "c")])

  it("works for simple case", {
    expect_setequal(
      outpack_search(bquote(usedby(.(ids["b"]))),
                    scope = quote(name == "a"),
                    root = root),
      ids["a"])
  })

  it("works with subqueries", {
    expect_setequal(
      outpack_search(quote(usedby({report_b})), # nolint
                    scope = quote(name == "a"),
                    subquery = list(report_b = quote(latest(name == "b"))),
                    root = root),
      ids["a"])
  })

  it("can return only immediate dependencies", {
    expect_setequal(
      outpack_search(quote(usedby({report_d}, 1)), # nolint
                    subquery = list(report_d = quote(latest(name == "d"))),
                    root = root),
      ids[c("b", "c")])
  })

  it("can use named arg", {
    expect_setequal(
      outpack_search(quote(usedby({report_d}, depth = 1)), # nolint
                    subquery = list(report_d = quote(latest(name == "d"))),
                    root = root),
      ids[c("b", "c")])
  })

  it("can recurse full tree", {
    res <- outpack_search(quote(usedby({report_d})), # nolint
                         subquery = list(report_d = quote(latest(name == "d"))),
                         root = root)
    expect_setequal(res, ids[c("a", "b", "c")])
    expect_length(res, 3) ## Packets are not counted twice
  })

  it("returns empty vector when id has no dependencies", {
    expect_equal(
      outpack_search(bquote(usedby(.(ids["a"]))),
                    root = root),
      character(0))
  })

  it("returns empty vector when id unknown", {
    expect_equal(
      outpack_search(quote(usedby("123")),
                    scope = quote(name == "a"),
                    root = root),
      character(0))
  })
})


test_that("usedby returns multiple ids when parent used twice", {
  root <- create_temporary_root(use_file_store = TRUE)
  id_a1 <- create_random_packet(root, "a", list(x = 1))
  id_a2 <- create_random_packet(root, "a", list(x = 1))
  id_b <- create_random_dependent_packet(root, "b", c(id_a1, id_a2))

  expect_setequal(
    outpack_search(quote(usedby({report_b})), # nolint
                  scope = quote(name == "a"),
                  subquery = list(report_b = quote(latest(name == "b"))),
                  root = root),
    c(id_a1, id_a2))
})


test_that("usedby output can be used in groupings", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 2)
  ids["c"] <- create_random_dependent_packet(root, "c", ids[c("a", "b")])

  expect_setequal(
    outpack_search(quote(usedby({report_c}) && name == "b"), # nolint
                  subquery = list(report_c = quote(latest(name == "c"))),
                  root = root),
    ids["b"])
})


test_that("usedby errors if given expression which could return multiple ids", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 2)
  ids["b"] <- create_random_dependent_packet(root, "b", ids["a"])

  expect_error(
    outpack_search(quote(usedby({report_b})), # nolint
                  subquery = list(report_b = quote(name == "b")),
                  root = root),
    paste0("usedby must be called on an expression guaranteed to return a ",
           "single ID. Try wrapping expression in `latest` or `single`.\n",
           "  - in usedby({report_b})"),
    fixed = TRUE)

  ## Suggested fix works
  expect_equal(
    outpack_search(quote(usedby(latest({report_b}))), # nolint
                  subquery = list(report_b = quote(name == "b")),
                  root = root),
    ids["a"], ignore_attr = "names")
})

test_that("usedby returns empty vector if usedby called with 0 ids", {
  root <- create_temporary_root(use_file_store = TRUE)

  expect_equal(
    outpack_search(quote(usedby({latest(name == "b")})), root = root), # nolint
    character(0))
})

test_that("usedby depth works as expected", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)

  expect_setequal(
    outpack_search(quote(
      usedby({latest(name == "c")}, depth = 1)), root = root), # nolint
    ids["b"])

  expect_setequal(
    outpack_search(quote(
      usedby({latest(name == "c")}, depth = 2)), root = root), # nolint
    ids[c("a", "b")])

  expect_setequal(
    outpack_search(quote(
      usedby({latest(name == "c")}, depth = Inf)), root = root), # nolint
    ids[c("a", "b")])
})


test_that("useful errors returned when scope is invalid type", {
  root <- create_temporary_root(use_file_store = TRUE)

  expect_error(
    outpack_search(quote(latest()), scope = "the scope", root = root),
    "Invalid input for `scope`, it must be a language expression.")
})


describe("outpack_search can search for packets which use another", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_dependent_packet(root, "d", ids[c("b", "c")])

  it("works for simple case", {
    expect_setequal(
      outpack_search(bquote(uses(.(ids["b"]))),
                    scope = quote(name == "c"),
                    root = root),
      ids["c"])
  })

  it("can return only immediate dependencies", {
    expect_setequal(
      outpack_search(quote(uses({report_a}, 1)), # nolint
                    subquery = list(report_a = quote(latest(name == "a"))),
                    root = root),
      ids["b"])
  })

  it("can use named arg", {
    expect_setequal(
      outpack_search(quote(uses({report_a}, depth = 2)), # nolint
                    subquery = list(report_a = quote(latest(name == "a"))),
                    root = root),
      ids[c("b", "c", "d")])
  })

  it("can recurse full tree", {
    res <- outpack_search(quote(uses({report_a})), # nolint
                         subquery = list(report_a = quote(latest(name == "a"))),
                         root = root)
    expect_setequal(res, ids[c("b", "c", "d")])
    expect_length(res, 3) ## Packets are not counted twice
  })

  it("returns empty vector when id has no dependencies", {
    expect_equal(
      outpack_search(bquote(uses(.(ids["d"]))),
                    root = root),
      character(0))
  })

  it("returns empty vector when id unknown", {
    expect_equal(
      outpack_search(quote(uses("123")),
                    scope = quote(name == "b"),
                    root = root),
      character(0))
  })
})


test_that("uses and usedby can be used together", {
  root <- create_temporary_root(use_file_store = TRUE)
  # nolint start
  ## Setup like
  ## A -> B -> C
  ##   \
  ##     V
  ## D -> E
  # nolint end
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_packet(root, "d")
  ids["e"] <- create_random_dependent_packet(root, "e", ids[c("a", "d")])

  ## We can get to C from E (up tree then down)
  expect_setequal(
    outpack_search(
      quote(uses(single(usedby(latest(name == "e")) && name == "a"))),
      scope = quote(name == "c"),
      root = root),
    ids["c"])

  ## We can get to E from C (up tree then down)
  expect_setequal(
    outpack_search(
      quote(uses(single(usedby(latest(name == "c")) && name == "a"))),
      scope = quote(name == "e"),
      root = root),
    ids["e"])

  ## We can get to A from D (down tree then up)
  expect_setequal(
    outpack_search(quote(usedby(single(uses(latest(name == "d"))))),
                  scope = quote(name == "a"),
                  root = root),
    ids["a"])

  ## We can get to D from A (down tree then up)
  expect_setequal(
    outpack_search(
      quote(usedby(single(uses(latest(name == "a")) && name == "e"))),
      scope = quote(name == "d"),
      root = root),
    ids["d"])

  ## We can get to D from C (up tree, then down, then up again)
  expect_setequal(
    outpack_search(
      quote(usedby(single(uses({a}) && name == "e"))), # nolint
      scope = quote(name == "d"),
      subquery = list(
        a = quote(single(usedby(latest(name == "c")) && name == "a"))),
      root = root),
    ids["d"])
})
