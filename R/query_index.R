query_index <- R6::R6Class(
  "query_index",
  cloneable = FALSE,

  public = list(
    index = NULL,
    root = NULL,

    initialize = function(root, index) {
      self$root <- root
      self$index <- index
      private$index_unfiltered <- self$index
      lockBinding("root", self)
    },

    filter = function(ids) {
      self$index <- self$index[self$index$id %in% ids, ]
    },

    get_index_unfiltered = function() {
      query_index$new(self$root, private$index_unfiltered)
    }
  ),

  private = list(
    index_unfiltered = NULL
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
