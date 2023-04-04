##' Copy files from a packet to anywhere. Similar to
##' [outpack::outpack_packet_use_dependency] except that this is not
##' used in an active packet context. You can use this function to
##' pull files from an outpack root to a directory outside of the
##' control of outpack, for example.
##'
##' @title Copy files from a packet
##'
##' @inheritParams outpack_packet_use_dependency
##'
##' @param dest The directory to copy into
##'
##' @return Nothing, invisibly. Primarily called for its side effect
##'   of copying files from a packet into the directory `dest`
##'
##' @export
outpack_copy_files <- function(id, files, dest, root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)

  assert_named(files, unique = TRUE)
  assert_relative_path(names(files), no_dots = TRUE)
  src <- unname(files)
  dst <- file.path(dest, names(files))
  validate_packet_has_file(root, id, src)

  file_export(root, id, src, dst)
  invisible()
}
