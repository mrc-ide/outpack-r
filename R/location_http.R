outpack_location_http <- R6::R6Class(
  "outpack_location_http",

  private = list(
    client = NULL
  ),

  public = list(
    initialize = function(url) {
      private$client <- outpack_http_client$new(url)
    },

    list = function() {
      dat <- private$client$get("/metadata/list", parse_json = TRUE)
      data_frame(
        packet = vcapply(dat, "[[", "packet"),
        time = as.POSIXct(vcapply(dat, "[[", "time"), tz = "UTC"),
        hash = vcapply(dat, "[[", "hash"))
    },

    metadata = function(packet_ids) {
      ret <- vcapply(packet_ids, function(id) {
        private$client$get(sprintf("/metadata/%s", id), parse_json = FALSE)
      })
      names(ret) <- packet_ids
      ret
    },

    fetch_file = function(hash, dest) {
      ## TODO: not totally obvious how we should set the progress here
      ## (currently always FALSE), possibly via some sort of option,
      ## possibly via whatever logging interface we come up with as we
      ## could turn it on via log levels.
      private$client$get(sprintf("/file/%s", hash), download = dest)
    }
  ))
