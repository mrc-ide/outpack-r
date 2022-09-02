## A bit of sleight of hand is needed here to avoid a bit of a
## circular dependency. We'll relax this later once we get a non-R
## version of this sorted out.
##
## The getNamespace(ns)$fn constructions are equivalent to ns::fn but
## will slide under R CMD check without warnings that would break the
## build.
##
## For an eventual CRAN release we'l need some other way of doing this
## though.
install_deps <- function() {
  if (!requireNamespace("outpack.server")) {
    message("Installing additional requirements")
    getNamespace("remotes")$install_github("mrc-ide/outpack.server",
                                           upgrade = FALSE)
  }
}


outpack_server <- function(path, validate = NULL, log_level = "info",
                           start = TRUE) {
  testthat::skip_on_os("windows") # does not work on gha due to install issues
  install_deps()
  create <- function(path, validate, log_level) {
    getNamespace("outpack.server")$api(path, validate, log_level)
  }
  args <- list(path = path, validate = validate, log_level = log_level)
  bg <- getNamespace("porcelain")$porcelain_background$new(create, args)
  if (start) {
    bg$start()
  }
  bg
}
