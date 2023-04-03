#' @title Query index
#'
#' @description
#' Class for managing the active index whilst evaluating a query
#' @keywords internal
query_index <- R6::R6Class(
  "query_index",
  cloneable = FALSE,

  public = list(
    #' @field index The packet index
    index = NULL,
    #' @field depends Named list of data frames. Names are packet ids, values
    #'   are packets dependend on by this packet id (i.e. its parents).
    depends = NULL,
    #' @field uses Named list of data frames. Names are packet ids, values
    #'   are packets which are used by this packet id (i.e. its children).
    uses = NULL,
    #' @field root The outpack root object
    root = NULL,

    #' @description
    #' Create a new query_index object
    #'
    #' @param root The outpack root object
    #' @param index The packet index as a data frame
    #' @param depends Named list of data frames. Names are packet ids, values
    #'   are packets dependend on by this packet id (i.e. its parents).
    #' @param uses Named list of data frames. Names are packet ids, values
    #'   are packets used by on by this packet id (i.e. its children). This is
    #'   the same data as `depends` but relationships flow in the other
    #'   direction.
    initialize = function(root, index, depends, uses) {
      self$root <- root
      self$index <- index
      self$depends <- depends
      self$uses <- uses
      lockBinding("root", self)
      lockBinding("depends", self)
      lockBinding("uses", self)
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
    #' Get the ids of packets which this packet depends to a specified level
    #'
    #' @param id The id of the packet to get parents of
    #' @param depth Depth of parents to get, `depth` 1 gets immediate parents
    #' `depth` 2 gets parents and parents of parents, `depth` Inf will
    #' recurse the whole tree to get all parents
    #' @return The ids of the parents of this packet
    get_packet_depends = function(id, depth) {
      deps <- private$get_dependencies(id, depth, self$depends)
      intersect(deps, self$index$id) %||% character(0)
    },

    #' @description
    #' Get the ids of packets which are used by this packet
    #'
    #' @param id The id of the packet to get children of
    #' @param depth Depth of children to get, `depth` 1 gets immediate children
    #' `depth` 2 gets children and children of children, `depth` Inf will
    #' recurse the whole tree to get all children
    #' @return The ids of the children of this packet
    get_packet_uses = function(id, depth) {
      deps <- private$get_dependencies(id, depth, self$uses)
      intersect(deps, self$index$id) %||% character(0)
    }
  ),

  private = list(
    get_dependencies = function(id, depth, dependency_data) {
      if (depth <= 0) {
        return(character(0))
      }
      deps <- dependency_data[[id]]$packet
      unique(c(deps, unlist(lapply(deps, private$get_dependencies,
                                   depth - 1, dependency_data))))
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
  uses <- build_packet_uses(depends)
  if (require_unpacked) {
    index <- index[index$id %in% idx$unpacked$packet, ]
    depends <- depends[names(depends) %in% index$id]
    uses <- uses[names(uses) %in% index$id]
  }
  query_index$new(root, index, depends, uses)
}

build_packet_uses <- function(dependencies) {
  ids <- names(dependencies)
  uses <- list()
  for (id in ids) {
    for (packet in dependencies[[id]]$packet) {
      if (is.null(uses[[packet]])) {
        uses[[packet]] <- list(packet = id)
      } else {
        uses[[packet]]$packet <- unique(c(uses[[packet]]$packet, id))
      }
    }
  }
  uses
}
