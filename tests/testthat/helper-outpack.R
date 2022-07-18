## Globally enable schema validation everywhere; really this should be
## disabled on CRAN, and only enabled if jsonvalidate is found.
options(outpack.schema_validate = TRUE)


create_random_packet <- function(root, name = "data", parameters = NULL) {
  src <- fs::dir_create(tempfile())
  on.exit(unlink(src, recursive = TRUE))
  saveRDS(runif(10), file.path(src, "data.rds"))
  id <- outpack_packet_start(src, name, parameters = parameters, root = root)$id
  outpack_packet_end()
  id
}


mock_metadata_depends <- function(id, depends = character(0)) {
  ret <- list(list(id = id, depends = data_frame(id = depends)))
  names(ret) <- id
  ret
}


## Creates a simple chain of packets a, b, c, ... that depend on each
## other.
create_random_packet_chain <- function(root, length) {
  src <- fs::dir_create(tempfile())
  on.exit(unlink(src, recursive = TRUE))

  id <- character()
  for (i in seq_len(length)) {
    nm <- letters[[i]]
    p <- file.path(src, nm)
    fs::dir_create(p)
    id[[nm]] <- outpack_packet_start(p, nm, root = root)$id

    if (i == 1) {
      saveRDS(runif(10), file.path(p, "data.rds"))
    } else {
      code <- sprintf("saveRDS(readRDS('input.rds') * %d, 'data.rds')", i)
      writeLines(code, file.path(p, "script.R"))
      outpack_packet_use_dependency(id[[letters[i - 1]]],
                                    c("input.rds" = "data.rds"))
      outpack_packet_run("script.R")
    }
    outpack_packet_end()
  }

  id
}
