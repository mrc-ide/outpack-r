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
  subquery_env <- new.env(parent = emptyenv())
  if (!is.null(subquery)) {
    assert_named(subquery)
    for (query in subquery) {
      assert_has_names(query, "expr")
    }
    subquery_env <- as.environment(subquery)
  }
  expr_parsed <- query_parse(expr, subquery_env, root)
  validate_parameters(pars)

  ## We will want to do this processing more generally later (in the
  ## root object, saved as part of the index), because it will be a
  ## waste to do this every time. Ideally it can be done incrementally
  ## too.
  idx <- root$index()
  i <- match(idx$location$location, root$config$location$id)
  location <- split(root$config$location$name[i], idx$location$packet)
  index <- data_frame(
    id = names(idx$metadata) %||% character(0),
    name = vcapply(idx$metadata, "[[", "name"),
    ## Wrap these in I() because they're list columns
    parameters = I(lapply(idx$metadata, "[[", "parameters")),
    location = I(location))

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
    index <- index[index$id %in% ids, ]
  } else if (require_unpacked) {
    index <- index[index$id %in% idx$unpacked$packet, ]
  }

  query_eval(expr_parsed, index, pars)
}


query_parse <- function(expr, subquery_env, root) {
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

  query_parse_expr(expr, expr, subquery_env, root)
}


query_functions <- list(
  group = list("(" = 1, "!" = 1, "&&" = 2, "||" = 2),
  test = list("==" = 2, "!=" = 2, "<" = 2, "<=" = 2, ">" = 2, ">=" = 2),
  other = list(latest = c(0, 1), single = 1, at_location = c(1, Inf), none = 0))


query_component <- function(type, expr, context, args, ...) {
  list(type = type, expr = expr, context = context, args = args, ...)
}


query_parse_expr <- function(expr, context, subquery_env, root) {
  type <- query_parse_check_call(expr, context, subquery_env)
  switch(type,
         test = query_parse_test(expr, context),
         group = query_parse_group(expr, context, subquery_env, root),
         latest = query_parse_latest(expr, context, subquery_env, root),
         single = query_parse_single(expr, context, subquery_env, root),
         at_location = query_parse_at_location(expr, context),
         subquery = query_parse_subquery(expr, context, subquery_env, root),
         none = query_parse_none(expr, context),
         ## normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_parse_test <- function(expr, context) {
  args <- lapply(expr[-1], query_parse_value, context)
  name <- deparse(expr[[1]])
  query_component("test", expr, context, args, name = name)
}


query_parse_group <- function(expr, context, subquery_env, root) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env, root)
  name <- deparse(expr[[1]])
  query_component("group", expr, context, args, name = name)
}


query_parse_latest <- function(expr, context, subquery_env, root) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env, root)
  query_component("latest", expr, context, args)
}


query_parse_none <- function(expr, context) {
  query_component("none", expr, context, args = NULL)
}


query_parse_single <- function(expr, context, subquery_env, root) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env, root)
  query_component("single", expr, context, args)
}


query_parse_at_location <- function(expr, context) {
  args <- as.list(expr[-1])
  if (!all(vlapply(args, is.character))) {
    query_parse_error(
      "All arguments to at_location() must be string literals",
      expr, context)
  }
  args <- lapply(args, query_parse_value, context)
  query_component("at_location", expr, context, args)
}


as_outpack_query_evaluated <- function(x) {
  structure(x, class = "outpack_query_evaluated")
}


is_outpack_query_evaluated <- function(x) {
  inherits(x, "outpack_query_evaluated")
}


query_parse_subquery <- function(expr, context, subquery_env, root) {
  name <- as.character(expr)
  subquery <- get(name, envir = subquery_env)
  ids <- if (is_outpack_query_evaluated(subquery)) {
    subquery_env[[name]]$result
  } else {
    subquery_env[[name]]$result <- as_outpack_query_evaluated(
      outpack_query(subquery$expr,
                    scope = subquery$scope,
                    root = root))
    subquery_env[[name]]$result
  }
  evaluated_expr <- if (length(ids) == 0) {
    quote(none())
  } else {
    exprs <- lapply(ids, function(id) {
      bquote(id == .(id))
    })
    query_build_or(exprs)
  }

  query_parse_expr(evaluated_expr,
                   context = context,
                   subquery_env = subquery_env,
                   root = root)
}


query_build_or <- function(exprs) {
  if (length(exprs) == 1) {
    return(exprs[[1]])
  }
  concatenated <- exprs[[1]]
  exprs <- exprs[-1]
  add_or <- function(exprs, concatenated) {
    concatenated <- call("||", concatenated, exprs[[1]])
    if (length(exprs[-1]) > 0) {
      add_or(exprs[-1], concatenated)
    } else {
      concatenated
    }
  }
  add_or(exprs, concatenated)
}


query_error <- function(msg, expr, context, prefix) {
  if (identical(expr, context)) {
    stop(sprintf("%s\n  - %s %s", msg, prefix, deparse_str(expr)),
         call. = FALSE)
  } else {
    width <- max(nchar(prefix), nchar("within"))
    stop(sprintf("%s\n  - %s %s\n  - %s %s",
                 msg,
                 format(prefix, width = width), deparse_str(expr),
                 format("within", width = width), deparse_str(context)),
         call. = FALSE)
  }
}


query_parse_error <- function(msg, expr, context) {
  query_error(msg, expr, context, "in")
}


query_eval_error <- function(msg, expr, context) {
  query_error(msg, expr, context, "while evaluating")
}


is_subquery <- function(expr, subquery_env) {
  is.name(expr) && exists(as.character(expr), where = subquery_env)
}


query_parse_check_call <- function(expr, context, subquery_env) {
  if (is_subquery(expr, subquery_env)) {
    return("subquery")
  }
  if (!is.call(expr)) {
    query_parse_error(sprintf(
      "Invalid query '%s'; expected some sort of expression",
      deparse_str(expr)),
      expr, context)
  }

  fn <- as.character(expr[[1]])

  if (fn %in% names(query_functions$group)) {
    type <- "group"
  } else if (fn %in% names(query_functions$test)) {
    type <- "test"
  } else {
    type <- "other"
  }
  len <- query_functions[[type]][[fn]]

  if (is.null(len)) {
    query_parse_error(sprintf(
      "Invalid query '%s'; unknown query component '%s'",
      deparse_str(expr), fn),
      expr, context)
  }

  nargs <- length(expr) - 1L
  if (length(len) == 1) {
    if (nargs != len) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected %d args but recieved %d",
        fn, len, nargs),
        expr, context)
    }
  } else {
    if (nargs < len[[1]]) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected at least %d args but recieved %d",
        fn, len[[1]], nargs),
        expr, context)
    }
    if (nargs > len[[2]]) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected at most %d args but recieved %d",
        fn, len[[2]], nargs),
        expr, context)
    }
  }

  if (type == "other") {
    type <- fn
  }

  type
}


query_parse_value <- function(expr, context) {
  if (is.numeric(expr) || is.character(expr) || is.logical(expr)) {
    list(type = "literal", value = expr)
  } else if (identical(expr, quote(name)) || identical(expr, quote(id))) {
    list(type = "lookup",
         name = deparse(expr))
  } else if (is_call(expr, ":")) {
    name <- deparse_str(expr[[2]])
    valid <- c("parameter", "this")
    if (!(name %in% valid)) {
        query_parse_error(sprintf(
          "Invalid lookup '%s'", name), expr, context)
    }
    list(type = "lookup",
         name = name,
         query = deparse_str(expr[[3]]))
  } else {
    query_parse_error(
      sprintf("Unhandled query expression value '%s'", deparse_str(expr)),
      expr, context)
  }
}


query_eval <- function(query, index, pars) {
  switch(query$type,
         literal = query$value,
         lookup = query_eval_lookup(query, index, pars),
         group = query_eval_group(query, index, pars),
         test = query_eval_test(query, index, pars),
         latest = query_eval_latest(query, index, pars),
         single = query_eval_single(query, index, pars),
         at_location = query_eval_at_location(query, index, pars),
         none = query_eval_none(query, index, pars),
         ## Normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_eval_latest <- function(query, index, pars) {
  if (length(query$args) == 0) {
    candidates <- index$id
  } else {
    candidates <- query_eval(query$args[[1]], index, pars)
  }
  if (length(candidates) == 0) NA_character_ else last(candidates)
}


query_eval_single <- function(query, index, pars) {
  candidates <- query_eval(query$args[[1]], index, pars)
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
  i <- vlapply(index$location, function(x) any(x %in% location))
  index$id[i]
}


query_eval_none <- function(query, index, pars) {
  NA_character_
}


query_eval_lookup <- function(query, index, pars) {
  switch(query$name,
         name = index$name,
         id = index$id,
         parameter = lapply(index$parameters, "[[", query$query),
         this = query_lookup_this(query$query, pars, query$expr, query$context),
         ## Normally unreachable
         stop("Unhandled lookup [outpack bug - please report]"))
}


query_eval_group <- function(query, index, pars) {
  args <- lapply(query$args, query_eval, index, pars)
  switch(query$name,
         "&&" = intersect(args[[1]], args[[2]]),
         "||" = union(args[[1]], args[[2]]),
         "!" = setdiff(index$id, args[[1]]),
         "(" = args[[1]],
         ## Normally unreachable
         stop("Unhandled operator [outpack bug - please report]"))
}


query_eval_test <- function(query, index, pars) {
  args <- lapply(query$args, query_eval, index, pars)
  i <- query_eval_test_binary(query$name, args[[1]], args[[2]])
  index$id[i]
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
