## Almost certainly this will get expanded, but this should work fine
## for our initial needs.
find_all_dependencies <- function(id, metadata) {
  ret <- unique(id)
  while (length(id) > 0) {
    id_new <- unlist(lapply(metadata[id], function(x) x$depends$packet),
                     FALSE, FALSE)
    id <- setdiff(id_new, ret)
    ret <- c(id, ret)
  }
  sort(ret)
}
