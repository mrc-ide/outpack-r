deparse_query <- function(x) {
  if (length(x) == 1) {
    return(deparse_single(x))
  }

  fn <- as.character(x[[1]])
  args <- x[-1]

  ## Note this includes invalid operators, even if they are invalid we
  ## still want to return formatted nicely
  prefix_operators <- list("!", "-")
  infix_operators <- list("!", "&&", "||", "==", "!=", "<", "<=", ">", ">=",
                            ":", "<-", "%in%", "+", "-", "*", "/", "&", "|")
  bracket_operators <- list("(" = ")", "{" = "}", "[" = "]")

  if (fn %in% infix_operators && length(args) == 2) {
    query_str <- deparse_infix(fn, args)
  } else if (fn %in% prefix_operators) {
    query_str <- deparse_prefix(fn, args)
  } else if (fn %in% names(bracket_operators)) {
    closing <- bracket_operators[[fn]]
    query_str <- deparse_brackets(fn, args, closing)
  } else {
    query_str <- deparse_regular_function(fn, args)
  }
  query_str
}

deparse_single <- function(x) {
  str <- as.character(x)
  if (is.character(x)) {
    str <- paste0('"', str, '"')
  } else if (is.call(x)) {
    str <- paste0(str, "()")
  }
  str
}

deparse_prefix <- function(fn, args) {
  deparse_regular_function(fn, args,
                           opening_bracket = "", closing_bracket = "")
}

deparse_infix <- function(fn, args) {
  sep <- if (fn == ":") "" else " "
  paste(deparse_query(args[[1]]), fn, deparse_query(args[[2]]), sep = sep)
}

deparse_brackets <- function(fn, args, closing) {
  if (fn == "[") {
    func <- args[[1]]
    args <- args[-1]
  } else {
    func <- ""
  }
  deparse_regular_function(func, args, fn, closing)
}

deparse_regular_function <- function(fn, args, opening_bracket = "(",
                                     closing_bracket = ")") {
  arg_str <- paste(vcapply(args, deparse_query), collapse = ", ")
  paste0(fn, opening_bracket, arg_str, closing_bracket)
}
