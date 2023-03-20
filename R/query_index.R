query_index <- R6::R6Class(
  "query_index",
  cloneable = FALSE,

  public = list(
    index = NULL,
    depends = NULL,
    root = NULL,

    initialize = function(root, index, depends) {
      self$root <- root
      self$index <- index
      private$index_unfiltered <- self$index
      self$depends <- depends
      lockBinding("root", self)
      lockBinding("depends", self)
    },

    filter = function(ids) {
      self$index <- self$index[self$index$id %in% ids, ]
    },

    get_index_unfiltered = function() {
      query_index$new(self$root, private$index_unfiltered, private$depends)
    },

    get_packet_depends = function(id, immediate) {
      if (immediate) {
        deps <- self$depends[[id]]$packet
      } else {
        deps <- unique(private$get_all_packet_depends(id))
      }
      deps <- deps[deps %in% self$index$id] ## filter on current index
      deps %||% character(0)
    }
  ),

  private = list(
    index_unfiltered = NULL,

    get_all_packet_depends = function(id) {
      deps <- self$depends[[id]]$packet
      c(deps, unlist(lapply(deps, private$get_all_packet_depends)))
    }
  )
)


new_query_index <- function(root, require_unpacked) {
  root <- outpack_root_open(root, locate = TRUE)
  idx <- root$index()
  i <- match(idx$location$location, root$config$location$id)
  location <- split(root$config$location$name[i], idx$location$packet)
  index <- data_frame(
    id = names(idx$metadata) %||% character(0),
    name = vcapply(idx$metadata, "[[", "name"),
    ## Wrap these in I() because they're list columns
    parameters = I(lapply(idx$metadata, "[[", "parameters")),
    location = I(location))
  depends <- lapply(idx$metadata, "[[", "depends")
  if (require_unpacked) {
    index <- index[index$id %in% idx$unpacked$packet, ]
    depends <- depends[names(depends) %in% index$id]
  }
  query_index$new(root, index, depends)
}
