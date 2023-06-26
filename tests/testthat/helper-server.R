outpack_server <- function(path) {
  outpack_server <- Sys.which("outpack_server")
  if (!nzchar(outpack_server)) {
    testthat::skip("outpack_server not installed")
  }
  args <- c("--root", path)
  px <- processx::process$new(outpack_server, args)
  withr::defer_parent(px$kill())
  px
}
