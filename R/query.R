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
    expr <- parse(text = expr)
  } else if (!is.language(expr)) {
    stop("invalid input")
  }

  query_parse_expr(expr)
}


query_group <- list("(" = 1, "!" = 1, "&&" = 2, "||" = 2)
query_test <- list("==" = 2, "!=" = 2, "<" = 2, "<=" = 2, ">" = 2, ">=" = 2)
query_other <- list("latest" = 1, "at_location" = c(1, Inf))


query_parse_expr <- function(expr) {
  fn <- query_parse_check_call(expr)
  if (fn %in% names(query_test)) {
    ret <- list(type = "test",
                name = fn,
                args = lapply(expr[-1], query_parse_value))
  } else if (fn %in% names(query_group)) {
    fn <- deparse(expr[[1]])
    list(type = "group",
         name = fn,
         args = lapply(expr[-1], query_parse_expr))
  } else if (fn == "latest") {
    ret <- list(type = "latest",
                args = lapply(expr[-1], query_parse_expr))
  } else if (fn == "at_location") {
    args <- as.list(expr[-1])
    if (!all(vlapply(args, is.character))) {
      stop("All arguments to at_location() must be string literals")
    }
    ret <- list(type = "at_location",
                args = lapply(args, query_parse_value)) # literal strings
  } else {
    stop("Unhandled") # TODO - we can never get here due to the above
  }
}


query_parse_check_call <- function(expr) {
  if (!is.call(expr)) {
    stop(sprintf(
      "Invalid query '%s'; expected some sort of expression",
      deparse_str(expr)),
      call. = FALSE)
  }

  len <- c(query_group, query_test, query_other)

  fn <- as.character(expr[[1]])
  if (!(fn %in% names(len))) {
    stop(sprintf(
      "Invalid query '%s'; unknown query component '%s'",
      deparse_str(expr), fn),
      call. = FALSE)
  }

  nargs <- length(expr) - 1L
  if (length(len[[fn]]) == 1) {
    if (nargs != len[[fn]]) {
      stop(sprintf(
        "Invalid call to %s() in %s, expected %d args but recieved %d",
        fn, deparse_str(expr), len[[fn]], nargs), call. = FALSE)
    }
  } else {
    if (nargs < len[[fn]][[1]]) {
      stop(sprintf(
        "Invalid call to %s() in %s, expected at least %d args but recieved %d",
        fn, deparse_str(expr), len[[fn]][[1]], nargs), call. = FALSE)
    }
    stopifnot(len[[fn]][[2]] == Inf) # not supported yet
  }

  fn
}


query_parse_value <- function(expr) {
  if (is.numeric(expr) || is.character(expr) || is.logical(expr)) {
    list(type = "literal", value = expr)
  } else if (identical(expr, quote(name))) {
    list(type = "lookup",
         name = "name")
  } else if (is_call(expr, ":")) {
    if (!identical(expr[[2]], quote(parameter))) {
        stop(sprintf(
          "Invalid query '%s'; only parameter: supported for now",
          deparse_str(expr)),
          call. = FALSE)
    }
    list(type = "lookup",
         name = "parameter",
         query = as.character(expr[[3]]))
  } else {
    ## TODO: this is not going to be very actionable without context;
    ## provide the parent expression and the component within it?
    stop("Unhandled value type")
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
         stop("you may be asking yourself, how did I get here?")) # TODO
}


query_eval_latest <- function(query, index) {
  candidates <- query_eval(query$args[[1]], index)
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
         stop("No such query")) # TODO
}


query_eval_group <- function(query, index) {
  args <- lapply(query$args, query_eval, index)
  switch(query$name,
         "&&" = intersect(args[[1]], args[[2]]),
         "||" = union(args[[1]], args[[2]]),
         "!" = setdiff(index$id, args[[1]]),
         "(" = args[[1]],
         stop("no such operator")) # TODO
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
