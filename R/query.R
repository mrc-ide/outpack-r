## We're going to support some sort of 'scope' restriction here that
## we'll put around all queries, limiting for example the name or
## location set.
outpack_query <- function(expr, scope = NULL, root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  expr_parsed <- query_parse(expr)

  ## We will want to do this more generally later, because it will be
  ## a waste to do this every time. We also need to get location on
  ## here too.
  index <- data_frame(
    id = names(root$index()$metadata),
    name = vcapply(root$index()$metadata, "[[", "name"),
    parameters = I(lapply(root$index()$metadata, "[[", "parameters")))

  if (!is.null(scope)) {
    ## Here, filter 'data' by additional queries
    browser()
  }

  ## TODO: Also may need to pass through the parameters data
  ## TODO: Something to "explain why X is not valid"
  query_eval(expr_parsed, index)
}


## The things we have are:
## filter: [set] -> [subset]
query_parse <- function(expr) {
  if (is.character(expr)) {
    expr <- parse(text = expr)
  } else if (!is.language(expr)) {
    stop("invalid input")
  }

  query_parse_expr(expr)
}


query_parse_expr <- function(expr) {
  if (is_call(expr, c("(", "!", "&&", "||"))) {
    query_parse_operator(expr)
  } else {
    query_parse_filter(expr)
  }
}


query_parse_operator <- function(expr) {
  fn <- deparse(expr[[1]])
  list(type = "operator",
       name = fn,
       args = lapply(expr[-1], query_parse_expr))
}


query_operators <- c("==", "!=", "<", "<=", ">", ">=")

query_parse_filter <- function(expr) {
  if (!is.call(expr)) {
    stop(sprintf(
      "Invalid query '%s'; expected some sort of expression",
      deparse_str(expr)),
      call. = FALSE)
  }

  len <- list(latest = 1)
  len[query_operators] <- 2

  fn <- as.character(expr[[1]])
  if (!(fn %in% names(len))) {
    stop(sprintf(
      "Invalid query '%s'; unknown filter '%s'",
      deparse(expr), fn),
      call. = FALSE)
  }

  ## Technically latest would work for anything?
  if (length(expr) - 1 != len[[fn]]) {
    stop(sprintf(
      "Invalid call to %s(), wrong number of args", # TODO: better message
      fn), call. = FALSE)
  }

  ret <- list(type = "filter",
              name = fn)

  if (fn %in% query_operators) {
    ## here we need the lhs and rhs to be accessors
    ret$args <- lapply(expr[-1], query_parse_value)
  } else if (fn == "latest") {
    ## arg is a subquery
    ret$args <- list(query_parse_expr(expr[[2]]))
  } else {
    stop("Can't get here?") # TODO
  }

  ret
}


query_parse_value <- function(expr) {
  if (is.numeric(expr) || is.character(expr) || is.logical(expr)) {
    return(list(type = "literal", value = expr))
  } else if (identical(expr, quote(name))) {
    list(type = "lookup",
         value = "name")
  } else if (is_call(expr, ":")) {
    if (!identical(expr[[2]], quote(parameter))) {
        stop(sprintf(
          "Invalid query '%s'; only parameter: supported for now",
          deparse_str(expr)),
          call. = FALSE)
    }
    list(type = "lookup",
         value = "parameter",
         query = as.character(expr[[3]]))
  } else {
    stop("Unhandled value type") # TODO
  }
}


query_operator_safe <- function(op, a, b) {
  op <- match.fun(op)
  vlapply(Map(function(a, b) !is.null(a) && !is.null(b) && op(a, b),
              a, b, USE.NAMES = FALSE),
          identity)
}


query_eval <- function(query, index) {
  if (query$type == "literal") {
    query$value
  } else if (query$type == "lookup") {
    if (query$value == "name") {
      index$name
    } else if (query$value == "parameter") {
      lapply(index$parameter, "[[", query$value$query)
    } else {
      stop("Impossible") # TODO
    }
  } else if (query$type == "operator") {
    args <- lapply(query$args, query_eval, index)

    switch(query$name,
           "&&" = intersect(args[[1]], args[[2]]),
           "||" = union(args[[1]], args[[2]]),
           "!" = setdiff(index$id, args[[1]]),
           "(" = args[[1]],
           stop("no such operator")) # TODO
  } else if (query$type == "filter" && query$name == "latest") {
    candidates <- query_eval(query$args[[1]], index)
    if (length(candidates) == 0) NA_character_ else last(candidates)
  } else if (query$type == "filter") {
    args <- lapply(query$args, query_eval, index)
    i <- query_operator_safe(query$name, args[[1]], args[[2]])
    index$id[i]
  } else {
    stop("you may be asking yourself, how did I get here?")
  }
}
