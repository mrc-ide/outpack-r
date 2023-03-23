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
    #' @field root The outpack root object
    root = NULL,

    #' @description
    #' Create a new query_index object
    #'
    #' @param root The outpack root object
    #' @param index The packet index as a data frame
    initialize = function(root, index) {
      self$root <- root
      self$index <- index
      private$index_unfiltered <- self$index
      private$index_scoped <- self$index
      lockBinding("root", self)
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
      query_index$new(self$root, private$index_unfiltered)
    },

    #' @description
    #' Get the scoped index, this is the index without any filtering but
    #' with scope rules applied.
    #'
    #' @return A new query_index object with the scoped index
    get_index_scoped = function() {
      query_index$new(self$root, private$index_scoped)
    }
  ),

  private = list(
    #' @field index_unfiltered The unfiltered unscoped index of all packets
    index_unfiltered = NULL,
    #' @field index_unfiltered The unfiltered index but scoped index of packets
    index_scoped = NULL
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
  if (require_unpacked) {
    index <- index[index$id %in% idx$unpacked$packet, ]
  }
  query_index$new(root, index)
}
