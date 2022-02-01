assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
  invisible(x)
}


assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be character", name), call. = FALSE)
  }
  invisible(x)
}


assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}


assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
  invisible(x)
}


assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
  invisible(x)
}


assert_file_exists <- function(x, check_case = TRUE, workdir = NULL,
                               name = "File") {
  if (!is.null(workdir)) {
    assert_scalar_character(workdir)
    owd <- setwd(workdir) # nolint
    on.exit(setwd(owd)) # nolint
  }

  if (!fs::file_exists(x)) {
    stop(sprintf("%s does not exist: '%s'", name, x))
  }
  if (check_case) {
    x_real <- fs::path_real(x)
    if (!fs::is_absolute_path(x)) {
      x_real <- fs::path_rel(x_real)
    }
    if (x_real != x) {
      stop(sprintf("%s does not exist: '%s' (should be '%s')",
                   name, x, x_real))
    }
  }

  invisible(x)
}
