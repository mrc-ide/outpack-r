## A dummy location for paths:
outpack_location_path <- R6::R6Class(
  "outpack_location_path",

  private = list(
    root = NULL
  ),

  public = list(
    initialize = function(path) {
      ## TODO: might allow this to filter which locations we send out
      ## as part of the configuration
      private$root <- outpack_root_open(path)
    },

    ##' @description Return time of last update
    last_update = function() {
      ## The only safe option here is to always update the index?
      index <- private$root$index_update(verbose = FALSE)
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
    pull_zip = function(id, dest) {
      browser()

      ## Here we'll use the usual temporary directory, but we'll want
      ## to allow specifying that later (probably on location
      ## creation?) in order to avoid copying across filesystems?

      ## If we've got these things checked out in different archive
      ## directories this will require some work.  If we're using a
      ## zip-based storage then we would want to return a zip.

      ## We might want to accept an optional list of skippable files
      ## (or hashes) here to avoid downloading too many things.  That
      ## would not be supported by all drivers (e.g., sharepoint).
    },

    pull_directory = function(id, dest) {
      dat <- private$root$index_update()
      ## TODO: dat$index should not have rownames
      found <- dat$index[dat$index$id == id, ]
      name <- found$name
      ## TODO: we don't have any way yet of configuring 'archive'
      ## here, and different outpack.  There's something missing here
      ## about tracking where things are unpacked to, or a general
      ## pattern for doing so.
      path <- file.path(private$root$root, "archive", name, id)

      stopifnot(file.exists(path))
      stopifnot(!file.exists(dest))

      fs::dir_copy(path, dest)
      invisible(dest)
    }
  ))
