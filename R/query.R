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
##'   referenced by name from the `expr`. Each subquery must be a list
##'   with at least an `expr` and optionally a `scope`.
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
  root <- outpack_root_open(root, locate = TRUE)
  if (!is.null(subquery)) {
    assert_named(subquery, unique = TRUE)
    subquery_env <- as.environment(subquery)
  } else {
    subquery_env <- new.env(parent = emptyenv())
  }
  expr_parsed <- query_parse(expr, expr, subquery_env)
  validate_parameters(pars)

  ## We will want to do this processing more generally later (in the
  ## root object, saved as part of the index), because it will be a
  ## waste to do this every time. Ideally it can be done incrementally
  ## too.
  index <- new_query_index(root, require_unpacked)
  if (!is.null(name)) {
    name_call <- call("==", quote(name), name)
    if (is.null(scope)) {
      scope <- name_call
    } else {
      scope <- call("&&", name_call, scope)
    }
  }

  if (!is.null(scope)) {
    ids <- outpack_query(scope, pars, scope = NULL,
                         require_unpacked = require_unpacked,
                         root = root)
    index$scope(ids)
  }

  query_eval(expr_parsed, index, pars, subquery_env)
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
  other = list(latest = c(0, 1), single = 1, at_location = c(1, Inf),
               usedby = c(1, 2)))


query_component <- function(type, expr, context, args, ...) {
  list(type = type, expr = expr, context = context, args = args, ...)
}


query_parse_expr <- function(expr, context, subquery_env) {
  type <- query_parse_check_call(expr, context)
  switch(type,
         test = query_parse_test(expr, context),
         group = query_parse_group(expr, context, subquery_env),
         latest = query_parse_latest(expr, context, subquery_env),
         single = query_parse_single(expr, context, subquery_env),
         at_location = query_parse_at_location(expr, context, subquery_env),
         subquery = query_parse_subquery(expr, context, subquery_env),
         usedby = query_parse_usedby(expr, context, subquery_env),
         ## normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_parse_test <- function(expr, context) {
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
    if (!exists(query_name, where = subquery_env)) {
      all_subqueries <- mget(ls(envir = subquery_env), subquery_env)
      named_subqueries <- vlapply(all_subqueries,
                                  function(x) !is_anonymous_subquery(x))
      available_subqueries <- all_subqueries[named_subqueries]
      if (length(available_subqueries) > 0) {
        available_queries <- paste0(
          "Available subqueries are '",
          paste0(names(available_subqueries), collapse = "', '"),
          "'.")
      } else {
        available_queries <- "No named subqueries provided."
      }
      query_parse_error(
        sprintf("Cannot locate subquery named '%s'. %s", query_name,
                available_queries),
        expr, context)
    }
  } else {
    query_name <- openssl::md5(deparse_query(subquery[[1]]))
    subquery_env[[query_name]] <- as_anonymous_subquery(subquery[[1]])
  }
  parsed_query <- query_parse(subquery_env[[query_name]], context, subquery_env)
  query_component("subquery", expr, context,
                  args = list(name = query_name,
                              subquery = parsed_query))
}


query_parse_usedby <- function(expr, context, subquery_env) {
  args <- as.list(expr[-1])
  if (length(args) == 2) {
    if (is.logical(args[[2]])) {
      args[[2]] <- query_parse_value(args[[2]], context, subquery_env)
    } else {
      query_parse_error(
        paste0("Second argument to usedby() must be boolean, ",
               "set TRUE to only search immediate dependencies. ",
               "Otherwise search will recurse the dependency tree."),
        expr, context)
    }
  } else {
    args[[2]] <- query_parse_value(FALSE, context, subquery_env)
  }
  args[[2]]$name <- "immediate"
  if (is.call(args[[1]])) {
    args[[1]] <- query_parse_expr(args[[1]], context, subquery_env)
  } else {
    args[[1]] <- query_parse_value(args[[1]], context, subquery_env)
  }
  query_component("usedby", expr, context, args)
}


query_error <- function(msg, expr, context, prefix) {
  if (identical(expr, context)) {
    stop(sprintf("%s\n  - %s %s", msg, prefix, deparse_query(expr)),
         call. = FALSE)
  } else {
    width <- max(nchar(prefix), nchar("within"))
    stop(sprintf("%s\n  - %s %s\n  - %s %s",
                 msg,
                 format(prefix, width = width), deparse_query(expr),
                 format("within", width = width), deparse_query(context)),
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
      deparse_query(expr)),
      expr, context)
  }

  fn <- as.character(expr[[1]])

  if (fn %in% names(query_functions$group)) {
    type <- "group"
  } else if (fn %in% names(query_functions$test)) {
    type <- "test"
  } else if (fn %in% names(query_functions$subquery)) {
    type <- "subquery"
  } else { # fn is in names(query_functions$other)
    type <- "other"
  }
  len <- query_functions[[type]][[fn]]

  if (is.null(len)) {
    query_parse_error(sprintf(
      "Invalid query '%s'; unknown query component '%s'",
      deparse_query(expr), fn),
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
    name <- deparse_query(expr[[2]])
    valid <- c("parameter", "this")
    if (!(name %in% valid)) {
        query_parse_error(sprintf(
          "Invalid lookup '%s'", name), expr, context)
    }
    list(type = "lookup",
         name = name,
         query = deparse_query(expr[[3]]),
         expr = expr,
         context = context)
  } else {
    query_parse_error(
      sprintf("Unhandled query expression value '%s'", deparse_query(expr)),
      expr, context)
  }
}


as_anonymous_subquery <- function(x) {
  structure(x, class = c("outpack_anonymous_subquery", class(x)))
}


is_anonymous_subquery <- function(x) {
  inherits(x, "outpack_anonymous_subquery")
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
         usedby = query_eval_usedby(query, index, pars, subquery_env),
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


query_eval_subquery <- function(query, index, pars, subquery_env) {
  name <- query$args$name
  subquery <- get(name, envir = subquery_env)
  if (is.null(subquery$result)) {
    subquery$result <- query_eval(query$args$subquery,
                                  index$get_index_scoped(),
                                  pars = NULL,
                                  subquery_env)
  }
  subquery$result
}


query_eval_usedby <- function(query, index, pars, subquery_env) {
  ## Eval usedby arg without scope, we need to find all packets which
  ## were usedby this one, so find parents without scope and apply scope
  ## later when finding the results of the main query.
  id <- query_eval(query$args[[1]], index$get_index_unfiltered(),
                   pars, subquery_env)
  len <- length(id)
  if (len == 0) {
    return(character(0))
  }
  if (len > 1) {
    query_eval_error(
      sprintf(paste0("Found %s ids in call to usedby, usedby can only work ",
                     "with a single id. Try wrapping enclosed query in 'latest' ",
                     "to ensure only one id is returned."), len),
      query$expr, query$context)
  }
  index$get_packet_depends(id, query$args[[2]]$value)
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
