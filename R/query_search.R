##' Evaluate a query against the outpack database, returning a vector
##' of matching packet ids.
##'
##' @title Query outpack's database
##'
##' @param ... Arguments passed through to [outpack_query], perhaps
##'   just a query expression
##'
##' @param parameters Optionally, a named list of parameters to substitute
##'   into the query (using the `this:` prefix)
##'
##' @param require_unpacked Logical, indicating if we should require
##'   that the packets are unpacked. If `FALSE` (the default) we
##'   search through all packets known to this outpack root,
##'   regardless of if they are locally available, but if `TRUE`, only
##'   unpacked packets will be considered.
##'
##' @param location Optionally, information to limit the search to. If
##'   `NULL`, all locations are considered. If non-`NULL`, this should
##'   be either a character vector of locations to consider, or a
##'   single numerical value indicating the minimum priority of th
##'   location.
##'
##' @param root The outpack root. Will be searched for from the
##'   current directory if not given.
##'
##' @return A character vector of matching ids
##' @export
outpack_search <- function(..., parameters = NULL, require_unpacked = FALSE,
                           location = NULL, root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  query <- as_outpack_query(...)
  outpack_query_eval(query, parameters, require_unpacked, location, root)
}


outpack_query_eval <- function(query, parameters, require_unpacked, location,
                               root) {
  assert_is(query, "outpack_query")
  assert_is(root, "outpack_root")
  validate_parameters(parameters)
  ## It's simple enough here to pre-compare the provided parameters
  ## with query$info$parameters, but we already have nicer error
  ## reporting at runtime that shows the context of where the
  ## parameter is used.
  index <- new_query_index(root, require_unpacked, location)
  query_eval(query$value, index, parameters, list2env(query$subquery))
}


query_eval <- function(query, index, parameters, subquery) {
  switch(query$type,
         literal = query$value,
         lookup = query_eval_lookup(query, index, parameters),
         group = query_eval_group(query, index, parameters, subquery),
         test = query_eval_test(query, index, parameters, subquery),
         latest = query_eval_latest(query, index, parameters, subquery),
         single = query_eval_single(query, index, parameters, subquery),
         subquery = query_eval_subquery(query, index, parameters, subquery),
         dependency = query_eval_dependency(query, index, parameters, subquery),
         ## Normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_eval_latest <- function(query, index, parameters, subquery) {
  if (length(query$args) == 0) {
    candidates <- index$index$id
  } else {
    candidates <- query_eval(query$args[[1]], index, parameters, subquery)
  }
  if (length(candidates) == 0) NA_character_ else last(candidates)
}


query_eval_single <- function(query, index, parameters, subquery) {
  candidates <- query_eval(query$args[[1]], index, parameters, subquery)
  len <- length(candidates)
  if (len == 0) {
    query_eval_error("Query did not find any packets",
                     query$expr, query$context)
  } else if (len > 1) {
    query_eval_error(
      sprintf("Query found %d packets, but expected exactly one", len),
      query$expr, query$context)
  }
  candidates
}


## TODO: we probably also need to make sure that none of this is
## recursive (e.g., subquery A referencing B etc; do that in the parse
## phase; things are now set up to support this).
query_eval_subquery <- function(query, index, parameters, subquery) {
  name <- query$args$name
  if (!subquery[[name]]$evaluated) {
    ## TODO: should we really not allow parameters here? Feels like
    ## they might be relevant?
    result <- query_eval(subquery[[name]]$parsed, index, parameters = NULL,
                         subquery)
    subquery[[name]]$result <- result
    subquery[[name]]$evaluated <- TRUE
  }
  subquery[[name]]$result
}


query_eval_dependency <- function(query, index, parameters, subquery) {
  ## Eval dependency arg without scope, we need to find all packets which
  ## were usedby or used in this one, so find parents/children without scope
  ## and apply scope later when finding the results of the main query.
  id <- query_eval(query$args[[1]], index, parameters, subquery)
  switch(query$name,
         usedby = index$get_packet_depends(id, query$args[[2]]$value),
         uses = index$get_packet_uses(id, query$args[[2]]$value))
}


query_eval_lookup <- function(query, index, parameters) {
  switch(query$name,
         name = index$index$name,
         id = index$index$id,
         parameter = lapply(index$index$parameters, "[[", query$query),
         this = query_eval_this(query$query, parameters, query$expr,
                                query$context),
         ## Normally unreachable
         stop("Unhandled lookup [outpack bug - please report]"))
}


query_eval_group <- function(query, index, parameters, subquery) {
  args <- lapply(query$args, query_eval, index, parameters, subquery)
  switch(query$name,
         "&&" = intersect(args[[1]], args[[2]]),
         "||" = union(args[[1]], args[[2]]),
         "!" = setdiff(index$index$id, args[[1]]),
         "(" = args[[1]],
         ## Normally unreachable
         stop("Unhandled operator [outpack bug - please report]"))
}


query_eval_test <- function(query, index, parameters, subquery) {
  args <- lapply(query$args, query_eval, index, parameters, subquery)
  i <- query_eval_test_binary(query$name, args[[1]], args[[2]])
  index$index$id[i]
}


query_eval_test_binary <- function(op, a, b) {
  op <- match.fun(op)
  ## Older versions of R do not allow mixing of zero and non-zero
  ## length inputs here, but we can do this ourselves:
  if (length(a) == 0 || length(b) == 0) {
    return(logical(0))
  }
  vlapply(Map(function(a, b) !is.null(a) && !is.null(b) && op(a, b),
              a, b, USE.NAMES = FALSE),
          identity)
}


query_eval_this <- function(name, parameters, expr, context) {
  if (!(name %in% names(parameters))) {
    msg <- sprintf("Did not find '%s' within given parameters (%s)",
                   name, paste(squote(names(parameters)), collapse = ", "))
    query_eval_error(msg, expr, context)
  }
  parameters[[name]]
}
