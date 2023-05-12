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
##' @param scope Optionally, a scope query to limit the packets
##'   searched by `pars`
##'
##' @param name Optionally, the name of the packet to scope the query on. This
##'   will be intersected with `scope` arg and is a shorthand way of running
##'   `scope = list(name = "name")`
##'
##' @param subquery Optionally, named list of subqueries which can be
##'   referenced by name from the `expr`.
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
outpack_query <- function(expr, pars = NULL, scope = NULL,
                          name = NULL, subquery = NULL,
                          require_unpacked = FALSE,
                          root = NULL) {
  ## TODO: pars -> parameters for consistency
  ## TODO: add locate arg for consistency?
  expr_parsed <- query_process(expr, scope, name, subquery)
  outpack_query_eval(expr_parsed, pars, require_unpacked, root)
}


outpack_query_eval <- function(expr, parameters, require_unpacked, root) {
  assert_is(expr, "outpack_query_processed")
  root <- outpack_root_open(root, locate = TRUE)
  validate_parameters(parameters)
  index <- new_query_index(root, require_unpacked)
  query_eval(expr$value, index, parameters, expr$subquery)
}


query_process <- function(expr, scope, name, subquery) {
  subquery_env <- make_subquery_env(subquery)
  expr_parsed <- query_parse(expr, expr, subquery_env)

  if (!is.null(name)) {
    name_call <- call("==", quote(name), name)
    if (is.null(scope)) {
      scope <- name_call
    } else {
      scope <- call("&&", name_call, scope)
    }
  }

  if (!is.null(scope)) {
    expr_parsed <- query_parse_add_scope(expr_parsed, scope)
  }

  single_value <- is_expr_single_value(expr_parsed, subquery_env)

  structure(list(value = expr_parsed,
                 single_value = single_value,
                 subquery = subquery_env),
            ## Stupid class name, will fix later
            class = "outpack_query_processed")
}


query_parse <- function(expr, context, subquery_env) {
  if (is.character(expr)) {
    if (length(expr) == 1 && grepl(re_id, expr)) {
      ## If we're given a single id, we construct a simple query with it
      expr <- bquote(single(id == .(expr)))
    } else {
      expr <- parse(text = expr, keep.source = FALSE)
      if (length(expr) != 1L) {
        stop("Expected a single expression")
      }
      expr <- expr[[1L]]
    }
  } else if (!is.language(expr)) {
    stop("Invalid input for query")
  }

  ## This is used extensively in orderly, so we'll support it here
  if (identical(expr, quote(latest))) {
    expr <- quote(latest())
  }

  if (is.null(context)) {
    context <- expr
  }

  query_parse_expr(expr, context, subquery_env)
}


query_functions <- list(
  group = list("(" = 1, "!" = 1, "&&" = 2, "||" = 2),
  test = list("==" = 2, "!=" = 2, "<" = 2, "<=" = 2, ">" = 2, ">=" = 2),
  subquery = list("{" = 1),
  dependency = list(usedby = c(1, 2), uses = c(1, 2)),
  other = list(latest = c(0, 1), single = 1, at_location = c(1, Inf)))


query_component <- function(type, expr, context, args, ...) {
  structure(
    list(type = type, expr = expr, context = context, args = args, ...),
    class = "outpack_query")
}


query_parse_expr <- function(expr, context, subquery_env) {
  type <- query_parse_check_call(expr, context)
  fn <- switch(type,
               test = query_parse_test,
               group = query_parse_group,
               latest = query_parse_latest,
               single = query_parse_single,
               at_location = query_parse_at_location,
               subquery = query_parse_subquery,
               dependency = query_parse_dependency,
               ## normally unreachable
               stop("Unhandled expression [outpack bug - please report]"))
  fn(expr, context, subquery_env)
}


query_parse_test <- function(expr, context, subquery_env) {
  args <- lapply(expr[-1], query_parse_value, context)
  name <- deparse(expr[[1]])
  query_component("test", expr, context, args, name = name)
}


query_parse_group <- function(expr, context, subquery_env) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env)
  name <- deparse(expr[[1]])
  query_component("group", expr, context, args, name = name)
}


query_parse_latest <- function(expr, context, subquery_env) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env)
  query_component("latest", expr, context, args)
}


query_parse_single <- function(expr, context, subquery_env) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env)
  query_component("single", expr, context, args)
}


query_parse_at_location <- function(expr, context, subquery_env) {
  args <- as.list(expr[-1])
  if (!all(vlapply(args, is.character))) {
    query_parse_error(
      "All arguments to at_location() must be string literals",
      expr, context)
  }
  args <- lapply(args, query_parse_value, context, subquery_env)
  query_component("at_location", expr, context, args)
}


query_parse_add_scope <- function(expr_parsed, scope) {
  if (!is.language(scope)) {
    stop("Invalid input for `scope`, it must be a language expression.")
  }

  parsed_scope <- query_parse(scope, scope, emptyenv())
  scoped_functions <- list("latest", "single")
  expr <- expr_parsed$expr

  if (expr_parsed$type %in% scoped_functions) {
    fn <- deparse(expr[[1]])
    ## Include scope inside the top level function call
    if (length(expr_parsed$args) == 0) {
      ## e.g. latest()
      expr_parsed$expr <- call(fn, scope)
      expr_parsed$args <- list(parsed_scope)
    } else {
      ## e.g. latest(name == "x")
      expr_parsed$expr <- call(fn, call("&&", expr[[-1]], scope))
      expr_parsed$args[[1]] <- query_component(
        "group", expr_parsed$expr, expr_parsed$expr,
        list(expr_parsed$args[[1]], parsed_scope), name = "&&")
    }
  } else {
    ## Include scope at end of expression
    expr_parsed$expr <- call("&&", expr, scope)
    expr_parsed <- query_component("group", expr_parsed$expr, expr_parsed$expr,
                                   list(expr_parsed, parsed_scope),
                                   name = "&&")
  }
  expr_parsed
}


is_named_subquery <- function(subquery) {
  ## Subquery is "named" if it is e.g. {sub}, otherwise it is
  ## anonymous e.g. {name == "x"}
  ## anonymous could also be length 1 e.g. `latest` so we need to account
  ## for this by checking if is.name
  sub <- subquery[[1]]
  all_funcs <- unlist(lapply(query_functions, names), use.names = FALSE)
  length(subquery) == 1 && length(sub) == 1 && is.name(sub)
}


query_parse_subquery <- function(expr, context, subquery_env) {
  subquery <- expr[-1]
  if (is_named_subquery(subquery)) {
    query_name <- deparse(subquery[[1]])
    if (is.null(subquery_env[[query_name]])) {
      named_subqueries <-
        names(which(vlapply(as.list(subquery_env), function(x) !x$anonymous)))
      if (length(named_subqueries) > 0) {
        available_queries <- sprintf(
          "Available subqueries are %s.",
          paste0(squote(sort(named_subqueries)), collapse = ", "))
      } else {
        available_queries <- "No named subqueries provided."
      }
      query_parse_error(
        sprintf("Cannot locate subquery named '%s'. %s", query_name,
                available_queries),
        expr, context)
    }
  } else {
    query_name <- add_subquery(NULL, subquery[[1]], context, subquery_env)
  }
  query_component("subquery", expr, context, args = list(name = query_name))
}


query_parse_dependency <- function(expr, context, subquery_env) {
  name <- deparse(expr[[1]])
  args <- as.list(expr[-1])
  if (length(args) == 2) {
    if (is.numeric(args[[2]]) && args[[2]] > 0) {
      args[[2]] <- query_parse_value(args[[2]], context, subquery_env)
    } else {
      query_parse_error(
        sprintf(paste(
          "`depth` argument in '%s()' must be a positive numeric, set",
          "to control the number of layers of parents to recurse through",
          "when listing dependencies. Use `depth = Inf` to search entire",
          "dependency tree."), name),
        expr, context)
    }
  } else {
    args[[2]] <- query_parse_value(Inf, context, subquery_env)
  }
  args[[2]]$name <- names(args[2]) %||% "depth"
  if (is.call(args[[1]])) {
    args[[1]] <- query_parse_expr(args[[1]], context, subquery_env)
    if (!is_expr_single_value(args[[1]], subquery_env)) {
      query_parse_error(
        sprintf(paste(
          "%s must be called on an expression guaranteed to return",
          "a single ID. Try wrapping expression in `latest` or `single`."),
          name),
        expr, context)
    }
  } else {
    args[[1]] <- query_parse_value(args[[1]], context, subquery_env)
  }
  query_component("dependency", expr, context, args, name = name)
}

## Check if a query component returns a single value
## Guaranteed single valued if one of the following is true
##   * it is function call to latest
##   * it is a function call to single
##   * it is an ID lookup
##   * it is a subquery whose expression is validates one of these conditions
is_expr_single_value <- function(parsed_expr, subquery_env) {
  if (parsed_expr$type == "subquery") {
    parsed_expr_sub <- subquery_env[[parsed_expr$args$name]]$parsed
    return(is_expr_single_value(parsed_expr_sub, subquery_env))
  }
  parsed_expr$type %in% c("latest", "single") ||
    (parsed_expr$type == "test" && (is_id_lookup(parsed_expr$args[[1]]) ||
                                    is_id_lookup(parsed_expr$args[[2]])))
}

is_id_lookup <- function(expr) {
  expr$type == "lookup" && expr$name == "id"
}


query_error <- function(msg, expr, context, prefix) {
  if (identical(expr, context)) {
    stop(sprintf("%s\n  - %s %s", msg, prefix, deparse_query(expr, NULL)),
         call. = FALSE)
  } else {
    width <- max(nchar(prefix), nchar("within"))
    stop(sprintf(
      "%s\n  - %s %s\n  - %s %s",
      msg,
      format(prefix, width = width), deparse_query(expr, NULL),
      format("within", width = width), deparse_query(context, NULL)),
      call. = FALSE)
  }
}


query_parse_error <- function(msg, expr, context) {
  query_error(msg, expr, context, "in")
}


query_eval_error <- function(msg, expr, context) {
  query_error(msg, expr, context, "while evaluating")
}


query_parse_check_call <- function(expr, context) {
  if (!is.call(expr)) {
    query_parse_error(sprintf(
      "Invalid query '%s'; expected some sort of expression",
      deparse_query(expr, NULL)),
      expr, context)
  }

  fn <- as.character(expr[[1]])

  if (fn %in% names(query_functions$group)) {
    type <- "group"
  } else if (fn %in% names(query_functions$test)) {
    type <- "test"
  } else if (fn %in% names(query_functions$subquery)) {
    type <- "subquery"
  } else if (fn %in% names(query_functions$dependency)) {
    type <- "dependency"
  } else { # fn is in names(query_functions$other)
    type <- "other"
  }
  len <- query_functions[[type]][[fn]]

  if (is.null(len)) {
    query_parse_error(sprintf(
      "Invalid query '%s'; unknown query component '%s'",
      deparse_query(expr, NULL), fn),
      expr, context)
  }

  nargs <- length(expr) - 1L
  if (length(len) == 1) {
    if (nargs != len) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected %d args but received %d",
        fn, len, nargs),
        expr, context)
    }
  } else {
    if (nargs < len[[1]]) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected at least %d args but received %d",
        fn, len[[1]], nargs),
        expr, context)
    }
    if (nargs > len[[2]]) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected at most %d args but received %d",
        fn, len[[2]], nargs),
        expr, context)
    }
  }

  if (type == "other") {
    type <- fn
  }

  type
}


query_parse_value <- function(expr, context, subquery_env) {
  if (is.numeric(expr) || is.character(expr) || is.logical(expr)) {
    list(type = "literal", value = expr)
  } else if (identical(expr, quote(name)) || identical(expr, quote(id))) {
    list(type = "lookup",
         name = deparse(expr))
  } else if (is_call(expr, ":")) {
    name <- deparse_query(expr[[2]], NULL)
    valid <- c("parameter", "this")
    if (!(name %in% valid)) {
        query_parse_error(sprintf(
          "Invalid lookup '%s'", name), expr, context)
    }
    list(type = "lookup",
         name = name,
         query = deparse_query(expr[[3]], NULL),
         expr = expr,
         context = context)
  } else {
    query_parse_error(
      sprintf("Unhandled query expression value '%s'",
              deparse_query(expr, NULL)),
      expr, context)
  }
}


## By looping through in order we'll prevent any circular dependencies
## between subqueries, though some work will possibly needed to make
## this obvious to the users - I think this is hard to accidentally
## trigger though.
make_subquery_env <- function(subquery) {
  if (!is.null(subquery)) {
    assert_named(subquery, unique = TRUE)
  }
  subquery_env <- new.env()
  for (nm in names(subquery)) {
    add_subquery(nm, subquery[[nm]], NULL, subquery_env)
  }
  subquery_env
}


subquery_env_to_list <- function(subquery_env) {
  nms <- names(subquery_env)
  if (length(nms) == 0) {
    return(NULL)
  }
  set_names(lapply(nms, function(x) subquery_env[[x]]$expr), nms)
}


add_subquery <- function(name, expr, context, subquery_env) {
  anonymous <- is.null(name)
  if (anonymous) {
    name <- openssl::md5(deparse_query(expr, NULL))
  }
  subquery_env[[name]] <- list(
    name = name,
    expr = expr,
    parsed = query_parse(expr, context, subquery_env),
    anonymous = anonymous,
    evaluated = FALSE,
    result = NULL)
  invisible(name)
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
         this = query_lookup_this(query$query, pars, query$expr, query$context),
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


query_lookup_this <- function(name, pars, expr, context) {
  if (!(name %in% names(pars))) {
    msg <- sprintf("Did not find '%s' within given pars (%s)",
                   name, paste(squote(names(pars)), collapse = ", "))
    query_eval_error(msg, expr, context)
  }
  pars[[name]]
}
