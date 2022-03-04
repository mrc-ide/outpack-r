## Globally enable schema validation everywhere; really this should be
## disabled on CRAN, and only enabled if jsonvalidate is found.
options(outpack.schema_validate = TRUE)

## This is not really meant for use; we'll need to derive a better set
## of primatives here later.  However, this might be enough to get
## us going.

## TODO: note that things like parameters etc are not yet pulled
## through here.

test_outpack_run <- function(path, name, script = "script.R",
                             depends = NULL, root = NULL) {
  root <- outpack_root_locate(root)

  tmp <- tempfile()
  fs::dir_copy(path, tmp)

  outpack_packet_start(tmp, name, root = root)
  withCallingHandlers({
    for (x in depends) {
      outpack_packet_use_dependency(x$id, x$files)
    }
    outpack_packet_run(script)
    outpack_packet_end()
  }, error = function(e) outpack_packet_cancel())
}
