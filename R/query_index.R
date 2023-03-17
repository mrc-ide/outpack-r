query_index <- R6::R6Class(
  "query_index",
  cloneable = FALSE,
  public = list(
    index = NULL,
    intial_index = NULL,
    root = NULL,

    initialize = function(root, require_unpacked) {
      self$root <- root
      idx <- self$root$index()
      i <- match(idx$location$location, root$config$location$id)
      location <- split(root$config$location$name[i], idx$location$packet)
      self$index <- data_frame(
        id = names(idx$metadata) %||% character(0),
        name = vcapply(idx$metadata, "[[", "name"),
        ## Wrap these in I() because they're list columns
        parameters = I(lapply(idx$metadata, "[[", "parameters")),
        location = I(location))
      if (require_unpacked) {
        index <- index[index$id %in% idx$unpacked$packet, ]
      }
      self$initial_index <- self$index
      lockBinding("initial_index", self)
      lockBinding("index", self)
      lockBinding("root", self)
    },

    filter_index <- function(ids) {
      self$index <- self$index[self$index$id %in% ids, ]
    }
  )
)
