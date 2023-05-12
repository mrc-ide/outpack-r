##' Evaluate a query against the outpack database, returning a vector
##' of matching packet ids.
##'
##' @title Query outpack's database
##'
##' @param expr The query expression
##'
##' @param pars Optionally, a named list of parameters to substitute
##'   into the query (using the `this:` prefix)
##'
##'
##' @param require_unpacked Logical, indicating if we should require
##'   that the packets are unpacked. If `FALSE` (the default) we
##'   search through all packets known to this outpack root,
##'   regardless of if they are locally available, but if `TRUE`, only
##'   unpacked packets will be considered.
##'
##' @param root The outpack root. Will be searched for from the
##'   current directory if not given.
##'
##' @return A character vector of matching ids
##' @export
outpack_search <- function(..., parameters = NULL, require_unpacked = FALSE,
                           root = NULL, locate = TRUE) {
  root <- outpack_root_open(root, locate)
  query <- as_outpack_query(...)
  outpack_query_eval(query, parameters, require_unpacked, root)
}


outpack_query_eval <- function(query, parameters, require_unpacked, root) {
  assert_is(query, "outpack_query")
  assert_is(root, "outpack_root")
  validate_parameters(parameters) # against query soon
  index <- new_query_index(root, require_unpacked)
  query_eval(query$value, index, parameters, list2env(query$subquery))
}


query_eval <- function(query, index, pars, subquery_env) {
  switch(query$type,
         literal = query$value,
         lookup = query_eval_lookup(query, index, pars),
         group = query_eval_group(query, index, pars, subquery_env),
         test = query_eval_test(query, index, pars, subquery_env),
         latest = query_eval_latest(query, index, pars, subquery_env),
         single = query_eval_single(query, index, pars, subquery_env),
         at_location = query_eval_at_location(query, index, pars),
         subquery = query_eval_subquery(query, index, pars, subquery_env),
         dependency = query_eval_dependency(query, index, pars, subquery_env),
         ## Normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_eval_latest <- function(query, index, pars, subquery_env) {
  if (length(query$args) == 0) {
    candidates <- index$index$id
  } else {
    candidates <- query_eval(query$args[[1]], index, pars, subquery_env)
  }
  if (length(candidates) == 0) NA_character_ else last(candidates)
}


query_eval_single <- function(query, index, pars, subquery_env) {
  candidates <- query_eval(query$args[[1]], index, pars, subquery_env)
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


query_eval_at_location <- function(query, index, pars) {
  location <- vcapply(query$args, "[[", "value")
  i <- vlapply(index$index$location, function(x) any(x %in% location))
  index$index$id[i]
}


## TODO: we probably also need to make sure that none of this is
## recursive (e.g., subquery A referencing B etc; do that in the parse
## phase; things are now set up to support this).
query_eval_subquery <- function(query, index, pars, subquery_env) {
  name <- query$args$name
  if (!subquery_env[[name]]$evaluated) {
    ## TODO: should we really not allow parameters here? Feels like
    ## they might be relevant?
    result <- query_eval(subquery_env[[name]]$parsed, index, pars = NULL,
                         subquery_env)
    subquery_env[[name]]$result <- result
    subquery_env[[name]]$evaluated <- TRUE
  }
  subquery_env[[name]]$result
}


query_eval_dependency <- function(query, index, pars, subquery_env) {
  ## Eval dependency arg without scope, we need to find all packets which
  ## were usedby or used in this one, so find parents/children without scope
  ## and apply scope later when finding the results of the main query.
  id <- query_eval(query$args[[1]], index, pars, subquery_env)
  len <- length(id)
  if (len == 0) {
    return(character(0))
  }
  switch(query$name,
         usedby = index$get_packet_depends(id, query$args[[2]]$value),
         uses = index$get_packet_uses(id, query$args[[2]]$value))
}


query_eval_lookup <- function(query, index, pars) {
  switch(query$name,
         name = index$index$name,
         id = index$index$id,
         parameter = lapply(index$index$parameters, "[[", query$query),
         this = query_eval_this(query$query, pars, query$expr, query$context),
         ## Normally unreachable
         stop("Unhandled lookup [outpack bug - please report]"))
}


query_eval_group <- function(query, index, pars, subquery_env) {
  args <- lapply(query$args, query_eval, index, pars, subquery_env)
  switch(query$name,
         "&&" = intersect(args[[1]], args[[2]]),
         "||" = union(args[[1]], args[[2]]),
         "!" = setdiff(index$index$id, args[[1]]),
         "(" = args[[1]],
         ## Normally unreachable
         stop("Unhandled operator [outpack bug - please report]"))
}


query_eval_test <- function(query, index, pars, subquery_env) {
  args <- lapply(query$args, query_eval, index, pars, subquery_env)
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


query_eval_this <- function(name, pars, expr, context) {
  if (!(name %in% names(pars))) {
    msg <- sprintf("Did not find '%s' within given pars (%s)",
                   name, paste(squote(names(pars)), collapse = ", "))
    query_eval_error(msg, expr, context)
  }
  pars[[name]]
}
