test_that("ids are valid", {
  id <- outpack_id()
  expect_silent(validate_outpack_id(id))
})


test_that("can detect malformed id", {
  expect_error(validate_outpack_id("myid"),
               "Malformed id 'myid'")
})
