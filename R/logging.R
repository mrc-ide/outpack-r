## TODO: need to also log the calling package etc so that we get
## decent feedback about orderly; get logging working there too.
outpack_log <- function(log_level, topic, detail, logger = NULL,
                        caller = NULL) {
  logger <- outpack_logger(logger)
  logger$log(log_level, topic, custom = list(detail = detail),
             caller = lgr::get_caller(-9))
}


outpack_log_info <- function(topic, detail, logger = NULL, caller = NULL) {
  outpack_log("info", topic, detail, logger, caller)
}


outpack_log_debug <- function(topic, detail, logger = NULL, caller = NULL) {
  outpack_log("debug", topic, detail, logger, caller)
}


outpack_log_set_threshold <- function(value, logger = NULL) {
  logger <- outpack_logger(logger)
  logger$set_threshold(value)
  invisible(logger)
}


outpack_log_set_console <- function(value, logger = NULL) {
  logger <- outpack_logger(logger)
  if (("console" %in% names(logger$appenders)) != value) {
    if (value) {
      logger$add_appender(outpack_console_appender(), name = "console")
    } else {
      logger$remove_appender("console")
    }
  }
  invisible(logger)
}


outpack_console_appender <- function() {
  lgr::AppenderConsole$new(
    layout = outpack_console_layout$new())
}


outpack_log_level <- function(level) {
  logger <- lgr::get_logger("outpack")
  logger$set_threshold(level)
  invisible()
}

outpack_console_layout <- R6::R6Class(
  "outpack_console_layout",
  inherit = lgr::Layout,

  public = list(
    format_event = function(event) {
      ## This is the original orderly log format, seems like a
      ## sensible one to use here, at least for now, as users are
      ## familar with it.  We'll sort out colouring here later, andou
      ## possibly use cli for the final format. For now though we just
      ## care about getting things out.
      topic <- event$msg
      detail <- event$custom$detail %||% ""
      if (length(detail) > 1) {
        browser()
      }
      ret <- trimws(sprintf("[ %s ]  %s", format(topic, width = 10), detail))
      if (is.null(ret) || !any(nzchar(ret))) {
        stop("preventing invalid log")
        browser()
      }
      ret
    }
  ))


logging_init <- function() {
  logger <- lgr::get_logger("outpack", reset = TRUE)
  logger$set_propagate(FALSE)
  logger$add_appender(outpack_console_appender(), name = "console")
  logger
}


new_packet_logger <- function(id, path) {
  stopifnot(fs::is_dir(path))
  logger <- lgr::get_logger(c("outpack", "packet", id))
  file_json <- tempfile("outpack-log-", fileext = ".json")
  logger$add_appender(lgr::AppenderJson$new(file_json), name = "json")
  logger
}


outpack_logger <- function(logger = NULL) {
  if (inherits(logger, "outpack_packet")) {
    logger$logger
  } else {
    logger %||% lgr::get_logger("outpack")
  }
}


## We might tweak this later
read_log_json <- function(path, ...) {
  lgr::read_json_lines(path, ...)
}
