test_that("queries can be deparsed", {
  expect_equal(deparse_query(quote(x)), "x")
  expect_equal(deparse_query(quote("x")), '"x"')
  expect_equal(deparse_query(quote('x')), '"x"') # nolint
  expect_equal(deparse_query(quote(2)), "2")
  expect_equal(deparse_query(quote(name == "name")), 'name == "name"')
  expect_equal(deparse_query(quote(id == "123")), 'id == "123"')
  expect_equal(deparse_query(quote(parameter:a == 2)), "parameter:a == 2")
  expect_equal(deparse_query(quote(latest)), "latest")
  expect_equal(deparse_query(quote(latest())), "latest()")
  expect_equal(deparse_query(quote(latest(a %in% b))), "latest(a %in% b)")
  expect_equal(deparse_query(quote(latest(parameter:x == 1 && name == "name"))),
               'latest(parameter:x == 1 && name == "name")')
  expect_equal(deparse_query(quote(latest(parameter:x == this:x))),
               "latest(parameter:x == this:x)")
  expect_equal(deparse_query(quote(latest({subquery}))), # nolint
               "latest({subquery})")
  expect_equal(deparse_query(quote(at_location("x", "y"))),
               'at_location("x", "y")')
  expect_equal(deparse_query(quote(latest(   ))), "latest()") # nolint
  expect_equal(deparse_query(quote(name    ==     "name")), 'name == "name"')
  expect_equal(deparse_query(quote(name == "other" && !(name == "data"))),
               'name == "other" && !(name == "data")')
  expect_equal(deparse_query(quote(x <- 23)), "x <- 23")
  expect_equal(deparse_query(quote(x[2])), "x[2]")
  expect_equal(deparse_query(quote(x[2, 3])), "x[2, 3]")
})
