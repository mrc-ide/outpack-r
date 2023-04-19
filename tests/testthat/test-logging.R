test_that("can log creation of outpack repo", {
  path <- temp_file()

  outpack_config_set(logging.console = FALSE, root)
  expect_output(
    root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE),
    "[ init       ]",
    fixed = TRUE)
})


test_that("can log basic packet running", {
  on.exit(outpack_packet_clear(), add = TRUE)
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()

  inputs <- c("data.csv", "script.R")
  env <- new.env()

  local_log_console_enable()
  out <- capture.output(
    p <- outpack_packet_start(path_src, "example", root = root))

  expect_length(out, 3)
  expect_equal(out[[1]], "[ name       ]  example")
  expect_equal(out[[2]], sprintf("[ id         ]  %s", p$id))
  expect_match(out[[3]], "^\\[ start")

  expect_output(
    outpack_packet_run("script.R", env, echo = FALSE, packet = p),
    "[ script     ]  script.R",
    fixed = TRUE)

  out <- capture.output(outpack_packet_end(packet = p))

  expect_length(out, 2)
  expect_match(out[[1]], "^\\[ end")
  expect_match(out[[2]], "^\\[ elapsed")

  expect_true(file.exists(file.path(path_src, "log.json")))
  dat <- read_log_json(file.path(path_src, "log.json"))

  expect_equal(nrow(dat), 6)
  expect_equal(
    dat$msg,
    c("name", "id", "start", "script", "end", "elapsed"))
  expect_equal(
    dat$caller,
    paste0("outpack_packet_", rep(c("start", "run", "end"), c(3, 1, 2))))
  expect_equal(
    dat$logger,
    rep(paste0("outpack/packet/", p$id), 6))
})
