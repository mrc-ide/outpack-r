## We're going to support some sort of 'scope' restriction here that
## we'll put around all queries, limiting for example the name or
## location set.
outpack_query <- function(expr, scope = NULL, root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  expr_parsed <- query_parse(expr)

  ## We will want to do this more generally later, because it will be
  ## a waste to do this every time.
  idx <- root$index()
  i <- match(idx$location$location, root$config$location$id)
  location <- split(root$config$location$name[i], idx$location$packet)
  index <- data_frame(
    id = names(root$index()$metadata),
    name = vcapply(root$index()$metadata, "[[", "name"),
    ## Wrap these in I() because they're list columns
    parameters = I(lapply(root$index()$metadata, "[[", "parameters")),
    location = I(location))

  if (!is.null(scope)) {
    ids <- outpack_query(scope, NULL, root)
    index <- index[index$id %in% ids, ]
  }

  ## TODO: Also may need to pass through the parameters data
  ## TODO: Something to "explain why X is not valid"
  query_eval(expr_parsed, index)
}


query_parse <- function(expr) {
  if (is.character(expr)) {
    expr <- parse(text = expr, keep.source = FALSE)
    if (length(expr) != 1L) {
      stop("Expected a single expression")
    }
    expr <- expr[[1L]]
  } else if (!is.language(expr)) {
    stop("Invalid input for query")
  }

  ## This is used extensively in orderly, so we'll support it here
  if (identical(expr, quote(latest))) {
    expr <- quote(latest())
  }

  query_parse_expr(expr, expr)
}


query_functions <- list(
  group = list("(" = 1, "!" = 1, "&&" = 2, "||" = 2),
  test = list("==" = 2, "!=" = 2, "<" = 2, "<=" = 2, ">" = 2, ">=" = 2),
  other = list(latest = c(0, 1), at_location = c(1, Inf)))


query_parse_expr <- function(expr, context) {
  type <- query_parse_check_call(expr, context)
  switch(type,
         test = query_parse_test(expr, context),
         group = query_parse_group(expr, context),
         latest = query_parse_latest(expr, context),
         at_location = query_parse_at_location(expr, context),
         ## normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_parse_test <- function(expr, context) {
  list(type = "test",
       name = deparse(expr[[1]]),
       args = lapply(expr[-1], query_parse_value, context))
}


query_parse_group <- function(expr, context) {
  list(type = "group",
       name = deparse(expr[[1]]),
       args = lapply(expr[-1], query_parse_expr, context))
}


query_parse_latest <- function(expr, context) {
  list(type = "latest",
       args = lapply(expr[-1], query_parse_expr, context))
}


query_parse_at_location <- function(expr, context) {
  args <- as.list(expr[-1])
  if (!all(vlapply(args, is.character))) {
    query_parse_error(
      "All arguments to at_location() must be string literals",
      expr, context)
  }
  list(type = "at_location",
       args = lapply(args, query_parse_value, context))
}


query_parse_error <- function(msg, expr, context) {
  if (identical(expr, context)) {
    stop(sprintf("%s\n  - in %s", msg, deparse_str(expr)),
         call. = FALSE)
  } else {
    stop(sprintf("%s\n  - in     %s\n  - within %s",
                 msg, deparse_str(expr), deparse_str(context)),
         call. = FALSE)
  }
}


query_parse_check_call <- function(expr, context) {
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
  } else { # fn is in names(query_functions$other)
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
  } else if (identical(expr, quote(name))) {
    list(type = "lookup",
         name = "name")
  } else if (is_call(expr, ":")) {
    if (!identical(expr[[2]], quote(parameter))) {
        query_parse_error(sprintf(
          "Invalid query '%s'; only parameter: supported for now",
          deparse_str(expr)), expr, context)
    }
    list(type = "lookup",
         name = "parameter",
         query = as.character(expr[[3]]))
  } else {
    query_parse_error(
      sprintf("Unhandled query expression value '%s'", deparse_str(expr)),
      expr, context)
  }
}


query_eval <- function(query, index) {
  switch(query$type,
         literal = query$value,
         lookup = query_eval_lookup(query, index),
         group = query_eval_group(query, index),
         test = query_eval_test(query, index),
         latest = query_eval_latest(query, index),
         at_location = query_eval_at_location(query, index),
         ## Normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_eval_latest <- function(query, index) {
  if (length(query$args) == 0) {
    candidates <- index$id
  } else {
    candidates <- query_eval(query$args[[1]], index)
  }
  if (length(candidates) == 0) NA_character_ else last(candidates)
}


query_eval_at_location <- function(query, index) {
  location <- vcapply(query$args, "[[", "value")
  i <- vlapply(index$location, function(x) any(x %in% location))
  index$id[i]
}


query_eval_lookup <- function(query, index) {
  switch(query$name,
         name = index$name,
         parameter = lapply(index$parameters, "[[", query$query),
         ## Normally unreachable
         stop("Unhandled lookup [outpack bug - please report]"))
}


query_eval_group <- function(query, index) {
  args <- lapply(query$args, query_eval, index)
  switch(query$name,
         "&&" = intersect(args[[1]], args[[2]]),
         "||" = union(args[[1]], args[[2]]),
         "!" = setdiff(index$id, args[[1]]),
         "(" = args[[1]],
         ## Normally unreachable
         stop("Unhandled operator [outpack bug - please report]"))
}


query_eval_test <- function(query, index) {
  args <- lapply(query$args, query_eval, index)
  i <- query_eval_test_binary(query$name, args[[1]], args[[2]])
  index$id[i]
}


query_eval_test_binary <- function(op, a, b) {
  op <- match.fun(op)
  vlapply(Map(function(a, b) !is.null(a) && !is.null(b) && op(a, b),
              a, b, USE.NAMES = FALSE),
          identity)
}
