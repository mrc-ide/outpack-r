## A dummy location for paths:
outpack_location_path <- R6::R6Class(
  "outpack_location_path",

  private = list(
    root = NULL
  ),

  public = list(
    initialize = function(path) {
      ## TODO: might allow this to filter which locations we send out
      private$root <- outpack_root_open(path)
    },

    ##' @description Return time of last update
    last_update = function() {
      index <- private$root$update_index()
      max_time(index$location$date)
    },

    ##' @description List ids, optionally since some previous time
    ##'
    ##' @param since Optional time.  This should be a time reported
    ##'   from *this* location and not simply a call to Sys.time()
    list = function(since = NULL) {
      dat <- private$root$index_update()$location
      id <- dat$id
      hash <- dat$hash
      if (!is.null(since)) {
        keep <- dat$date > since
        id <- dat$id[keep]
        hash <- dat$hash[keep]
      }

      list(time = max_time(dat$date), id = id, hash = hash)
    },

    ##' @description Fetch metadata given a list of ids, returning a list of
    ##'   strings with json-encoded data.
    ##'
    ##' @param ids A list of identifiers
    metadata = function(ids) {
      ## TODO: $root$root -> $root$path
      paths <- file.path(private$root$root, ".outpack", "metadata",
                         paste0(ids, ".json"))
      lapply(paths, read_string)
    },

    ##' @description Pull single packet
    ##'
    ##' @param id An identifier
    pull = function(id) {
      browser()

      ## If we've got these things checked out in different archive
      ## directories this will require some work.  If we're using a
      ## zip-based storage then we would want to return a zip.
    }
  ))
