test_that("can compute checksum", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(tmp))
  hash <- outpack_checksum(root = tmp)
  expect_equal(hash_parse(hash)$algorithm, "sha256")
  expect_equal(hash, hash_data(paste(ids, collapse = "")), "sha256")
})

test_that("can compute checksum on empty root", {
  tmp <- temp_file()
  root <- outpack_init(tmp, use_file_store = TRUE)
  hash <- outpack_checksum("md5", root = tmp)
  expect_equal(hash, "md5:d41d8cd98f00b204e9800998ecf8427e")
})
