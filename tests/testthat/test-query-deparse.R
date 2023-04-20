test_that("queries can be deparsed", {
  expect_equal(outpack_query_format(quote(x)), "x")
  expect_equal(outpack_query_format(quote("x")), '"x"')
  expect_equal(outpack_query_format(quote('x')), '"x"') # nolint
  expect_equal(outpack_query_format(quote(2)), "2")
  expect_equal(outpack_query_format(quote(TRUE)), "TRUE")
  expect_equal(outpack_query_format(quote(name == "name")), 'name == "name"')
  expect_equal(outpack_query_format(quote(id == "123")), 'id == "123"')
  expect_equal(outpack_query_format(quote(parameter:a == 2)),
               "parameter:a == 2")
  expect_equal(outpack_query_format(quote(latest)), "latest")
  expect_equal(outpack_query_format(quote(latest())), "latest()")
  expect_equal(outpack_query_format(quote(latest(a %in% b))),
               "latest(a %in% b)")
  expect_equal(
    outpack_query_format(quote(latest(parameter:x == 1 && name == "name"))),
    'latest(parameter:x == 1 && name == "name")')
  expect_equal(outpack_query_format(quote(latest(parameter:x == this:x))),
               "latest(parameter:x == this:x)")
  expect_equal(outpack_query_format(quote(latest({subquery}))), # nolint
               "latest({subquery})")
  expect_equal(outpack_query_format(quote(at_location("x", "y"))),
               'at_location("x", "y")')
  expect_equal(outpack_query_format(quote(latest(   ))), "latest()") # nolint
  expect_equal(outpack_query_format(quote(name    ==     "name")),
               'name == "name"')
  expect_equal(
    outpack_query_format(quote(name == "other" && !(name == "data"))),
    'name == "other" && !(name == "data")')
  expect_equal(outpack_query_format(quote(x <- 23)), "x <- 23")
  expect_equal(outpack_query_format(quote(x[2])), "x[2]")
  expect_equal(outpack_query_format(quote(x[2, 3])), "x[2, 3]")
  expect_equal(outpack_query_format(quote(!x)), "!x")
  expect_equal(outpack_query_format(quote(!(x))), "!(x)")
  expect_equal(outpack_query_format(quote(!(x || y))), "!(x || y)")
  expect_equal(outpack_query_format(quote(usedby("thing", -2))),
               'usedby("thing", -2)')
})
