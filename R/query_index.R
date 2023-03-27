#' @title Query index
#'
#' @description
#' Class for managing the active index whilst evaluating a query
#' @keywords internal
query_index <- R6::R6Class(
  "query_index",
  cloneable = FALSE,

  public = list(
    #' @field index The active index
    index = NULL,
    #' @field depends Named list of data frames. Names are packet ids, values
    #'   are packets dependend on by this packet id (i.e. its parents).
    depends = NULL,
    #' @field root The outpack root object
    root = NULL,

    #' @description
    #' Create a new query_index object
    #'
    #' @param root The outpack root object
    #' @param index The packet index as a data frame
    #' @param depends Named list of data frames. Names are packet ids, values
    #'   are packets dependend on by this packet id (i.e. its parents).
    initialize = function(root, index, depends) {
      self$root <- root
      self$index <- index
      private$index_unfiltered <- self$index
      private$index_scoped <- self$index
      self$depends <- depends
      lockBinding("root", self)
      lockBinding("depends", self)
    },

    #' @description
    #' Scope the index. This will filter the active index on the scope
    #' and save the scoped index for use later
    #'
    #' @param ids The ids to scope the index on
    #' @return Nothing, called for side effect
    scope = function(ids) {
      self$filter(ids)
      private$index_scoped <- self$index
      invisible(TRUE)
    },

    #' @description
    #' Filter the index. This will filter the active index on the ids
    #'
    #' @param ids The ids to filter the index on
    #' @return Nothing, called for side effect
    filter = function(ids) {
      self$index <- self$index[self$index$id %in% ids, ]
      invisible(TRUE)
    },

    #' @description
    #' Get the unfiltered & unscoped index, this is the index as it was
    #' when this object was created, before any filtering on scoping.
    #'
    #' @return A new query_index object with the unfiltered index
    get_index_unfiltered = function() {
      query_index$new(self$root, private$index_unfiltered, private$depends)
    },

    #' @description
    #' Get the scoped index, this is the index without any filtering but
    #' with scope rules applied.
    #'
    #' @return A new query_index object with the scoped index
    get_index_scoped = function() {
      query_index$new(self$root, private$index_scoped, private$depends)
    },

    #' @description
    #' Get the ids of packets which this packet depends to a specified level
    #'
    #' @param id The id of the packet to get parents of
    #' @param depth Depth of parents to get, `depth` 1 gets immediate parents
    #' `depth` 2 gets parents and parents of parents, `depth` Inf will
    #' recurse the whole tree to get all parents
    #' @return The ids of the parents of this packet
    get_packet_depends = function(id, depth) {
      deps <- unique(private$get_all_packet_depends(id, depth))
      intersect(deps, self$index$id) %||% character(0)
    }
  ),

  private = list(
    # The unfiltered unscoped index of all packets
    index_unfiltered = NULL,
    # The unfiltered index but scoped index of packets
    index_scoped = NULL,

    get_all_packet_depends = function(id, depth) {
      if (depth == 0) {
        return(character(0))
      }
      deps <- self$depends[[id]]$packet
      c(deps, unlist(lapply(deps, private$get_all_packet_depends, depth - 1)))
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
