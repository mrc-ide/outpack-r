## Globally enable schema validation everywhere; really this should be
## disabled on CRAN, and only enabled if jsonvalidate is found.
options(outpack.schema_validate = TRUE)


create_random_packet <- function(root, name = "data", parameters = NULL) {
  src <- fs::dir_create(tempfile())
  on.exit(unlink(src, recursive = TRUE))
  saveRDS(runif(10), file.path(src, "data.rds"))
  p <- outpack_packet_start(src, name, parameters = parameters, root = root)
  outpack_packet_end(p)
  p$id
}


create_deterministic_packet <- function(root, name = "data",
                                        parameters = NULL) {
  src <- fs::dir_create(tempfile())
  on.exit(unlink(src, recursive = TRUE))
  saveRDS(1:10, file.path(src, "data.rds"))
  p <- outpack_packet_start(src, name, parameters = parameters, root = root)
  outpack_packet_end(p)
  p$id
}


mock_metadata_depends <- function(id, depends = character(0)) {
  ret <- list(list(id = id, depends = data_frame(packet = depends)))
  names(ret) <- id
  ret
}


## Creates a simple chain of packets a, b, c, ... that depend on each
## other.
create_random_packet_chain <- function(root, length, base = NULL) {
  src <- fs::dir_create(tempfile())
  on.exit(unlink(src, recursive = TRUE), add = TRUE)

  id <- character()
  for (i in seq_len(length)) {
    nm <- letters[[i]]
    p <- file.path(src, nm)
    fs::dir_create(p)
    packet <- outpack_packet_start(p, nm, root = root)
    id[[nm]] <- packet$id

    if (i == 1 && is.null(base)) {
      saveRDS(runif(10), file.path(p, "data.rds"))
    } else {
      code <- sprintf("saveRDS(readRDS('input.rds') * %d, 'data.rds')", i)
      writeLines(code, file.path(p, "script.R"))
      id_use <- if (i > 1) id[[letters[i - 1]]] else base
      outpack_packet_use_dependency(packet, id_use, c("input.rds" = "data.rds"))
      outpack_packet_run(packet, "script.R")
    }
    outpack_packet_end(packet)
  }

  id
}


create_random_dependent_packet <- function(root, name, dependency_ids) {
  src <- fs::dir_create(tempfile())
  on.exit(unlink(src, recursive = TRUE), add = TRUE)

  p <- outpack_packet_start(src, name, root = root)

  len <- length(dependency_ids)
  if (len == 0) {
    saveRDS(runif(10), file.path(p, "data.rds"))
  } else {
    inputs <- paste0(sprintf("readRDS('input%s.rds')", seq_len(len)),
                     collapse = " * ")
    code <- sprintf("saveRDS(%s , 'data.rds')", inputs)
    writeLines(code, file.path(src, "script.R"))
    for (num in seq_len(len)) {
      input_name <- sprintf("input%s.rds", num)
      outpack_packet_use_dependency(p, dependency_ids[[num]],
                                    stats::setNames("data.rds", input_name))
    }
    outpack_packet_run(p, "script.R")
  }
  outpack_packet_end(p)

  p$id
}


create_temporary_root <- function(...) {
  path <- tempfile()
  withr::defer_parent(unlink(path, recursive = TRUE))
  outpack_init(path, ..., logging_console = FALSE)
}


## A really simple example that we use in a few places
create_temporary_simple_src <- function() {
  path <- tempfile()
  withr::defer_parent(unlink(path, recursive = TRUE))
  fs::dir_create(path)

  path <- tempfile()
  fs::dir_create(path)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('zzz.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path, "data.csv"),
            row.names = FALSE)

  path
}
