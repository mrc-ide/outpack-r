## NOTE: none of the auth bits here are done yet - we have a system in
## the orderlyweb client that lets us do this in a fairly pluggable
## way, supporting username/password and token based auth (with the
## former getting a token via username/password)
outpack_http_client <- R6::R6Class(
  "outpack_http_client",

  public = list(
    url = NULL,

    initialize = function(url) {
      self$url <- url
    },

    get = function(path, ...) {
      http_client_request(httr::GET, self$url, path, ...)
    }
  ))


http_client_request <- function(verb, server, path, ..., parse_json = TRUE, download = NULL) {
  if (is.null(download)) {
    response <- verb(paste0(server, path), ...)
  } else {
    response <- verb(paste0(server, path), ...,
                     http_client_download_options(download))
  }

  http_client_handle_error(response)
  if (is.null(download)) {
    txt <- httr::content(response, "text", encoding = "UTF-8")
    if (parse_json) {
      from_json(txt)$data
    } else {
      strip_response_wrapper(txt)
    }
  } else {
    download
  }
}


## This could probably be simplified considerably, it was designed to
## cope with something like docker or vault where we were less in
## control of the errors, and we can always put back in some better
## support later.
http_client_handle_error <- function(response) {
  code <- httr::status_code(response)
  if (code >= 400) {
    txt <- httr::content(response, "text", encoding = "UTF-8")
    res <- from_json(txt)
    stop(http_client_error(res$errors[[1]]$detail, code, res$errors))
  }
  response
}


http_client_error <- function(msg, code, errors) {
  err <- list(message = msg, errors = errors, code = code)
  class(err) <- c("outpack_http_client_error", "error", "condition")
  err
}


http_client_download_options <- function(dest) {
  c(httr::write_disk(dest),
    httr::accept("application/octet-stream"))
}


## when we get something back from the server it will be an object
##
## {
##   "status": "success",
##   "errors": null,
##   "data:" <payload>
## }
##
## and we want to get this payload. We can't deserialise though
## because we can't generally do a roundtrip into R objects and back
## losslessly!
##
## The opions here are
##
## * change the server to cope with an accept = "text/plain" header to
##   avoid adding the wrapper, which feels a bit annoying
##
## * take the porcelain approach and use V8 to pull out the component
##   that we want (json_parse_extract)
##
## * make some dirty assumptions here about how the formatting looks
##   and do it with regular expressions.
##
## We'll take the third approach here for now, as it's pretty
## straightforward really.
strip_response_wrapper <- function(txt) {
  ## Could also make this whitespace insensitive by adding some '\\s*'
  ## at every gap, but that's not likely to be needed.
  re <- paste0("^\\{",
               '("status":"success",|"errors":null,)*',
               '"data":(.+?)',
               '(,"status":"success"|,"errors":null)*',
               "\\}$")
  sub(re, "\\2", txt)
}
