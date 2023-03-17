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


assert_numeric <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
  invisible(x)
}


assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be logical", name), call. = FALSE)
  }
  invisible(x)
}


assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}


assert_scalar_numeric <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
}


assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
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


assert_relative_path <- function(x, no_dots = FALSE,
                                 name = deparse(substitute(x))) {
  err <- fs::is_absolute_path(x)
  if (any(err)) {
    stop(sprintf("'%s' must be relative %s",
                 name, ngettext(length(x), "path", "paths")),
         call. = FALSE)
  }
  if (no_dots && any(grepl("..", x, fixed = TRUE))) {
    stop(sprintf("'%s' must not contain '..' path components", name),
         call. = FALSE)
  }
}


assert_file_exists <- function(x, workdir = NULL, name = "File") {
  if (!is.null(workdir)) {
    assert_scalar_character(workdir)
    owd <- setwd(workdir) # nolint
    on.exit(setwd(owd)) # nolint
  }

  ok <- fs::file_exists(x)
  if (!all(ok)) {
    stop(sprintf("%s does not exist: %s",
                 name, paste(squote(x[!ok]), collapse = ", ")))
  }

  ## TODO: we should (as orderly does) verify that the casing is
  ## correct here (avoiding README.MD/README.md confusion), but that's
  ## actually quite hard to pull off.  Either we need to pull in the
  ## enormous mess of code in orderly, or we can just about achive
  ## this by looking at the fs::path_real(x) and stripping leading
  ## path parts with fs::path_rel().  This does do very badly in the
  ## case of symlinks though (e.g., macOS tempdir) and windows
  ## shortened paths (e.g., Windows - orderly struggles there
  ## generally too). (See 1170cc9)

  invisible(x)
}


assert_directory_does_not_exist <- function(x, name = "Directory") {
  ok <- !fs::dir_exists(x)
  if (!all(ok)) {
    stop(sprintf("%s already exists: %s",
                 name, paste(squote(x[!ok]), collapse = ", ")))
  }

  ## TODO: we should (as orderly does) verify that the casing is
  ## correct here (avoiding README.MD/README.md confusion), but that's
  ## actually quite hard to pull off.  Either we need to pull in the
  ## enormous mess of code in orderly, or we can just about achive
  ## this by looking at the fs::path_real(x) and stripping leading
  ## path parts with fs::path_rel().  This does do very badly in the
  ## case of symlinks though (e.g., macOS tempdir) and windows
  ## shortened paths (e.g., Windows - orderly struggles there
  ## generally too). (See 1170cc9)

  invisible(x)
}


assert_directory <- function(x, workdir = NULL, name = "Directory") {
  assert_file_exists(x, workdir, name)
  if (!fs::is_dir(x)) {
    stop(sprintf("%s must be a directory: '%s'", name, x))
  }
}



match_value <- function(arg, choices, name = deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(squote(choices), collapse = ", ")),
         call. = FALSE)
  }
  arg
}


assert_has_names <- function(x, required, name = deparse(substitute(x))) {
  msg <- setdiff(required, names(x))
  if (length(msg) > 0) {
    stop(sprintf("Required names missing from '%s': %s",
                 name, paste(squote(msg), collapse = ", ")))
  }
  invisible(x)
}
