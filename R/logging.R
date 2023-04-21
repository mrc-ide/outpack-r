## TODO: need to also log the calling package etc so that we get
## decent feedback about orderly; get logging working there too.
outpack_log <- function(log_level, topic, detail, logger, caller) {
  if (inherits(logger, "outpack_packet") || inherits(logger, "outpack_root")) {
    logger <- logger$logger
  }
  assert_is(logger, "Logger")
  logger$log(log_level, topic, custom = list(detail = detail), caller = caller)
}


outpack_log_info <- function(topic, detail, logger = NULL, caller = NULL) {
  outpack_log("info", topic, detail, logger, caller)
}


outpack_log_debug <- function(topic, detail, logger = NULL, caller = NULL) {
  outpack_log("debug", topic, detail, logger, caller)
}


outpack_console_appender <- function() {
  lgr::AppenderConsole$new(
    layout = outpack_console_layout$new(),
    filters = list(filter_drop_script_output))
}


filter_drop_script_output <- function(event) {
  !(event$caller == "outpack::outpack_packet_run" && event$msg == "output")
}


outpack_console_layout <- R6::R6Class(
  "outpack_console_layout",
  inherit = lgr::Layout,

  public = list(
    format_event = function(event) {
      ## This is the original orderly log format, seems like a
      ## sensible one to use here, at least for now, as users are
      ## familar with it.  We'll sort out colouring here later, and/or
      ## possibly use cli for the final format. For now though we just
      ## care about getting things out.
      topic <- event$msg
      detail <- event$custom$detail %||% ""
      stopifnot(length(detail) == 1)
      trimws(sprintf("[ %s ]  %s", format(topic, width = 10), detail))
    }
  ))


new_root_logger <- function(id, config) {
  logger <- lgr::get_logger(c("outpack", id))
  logger_configure(logger, config$logging$console, config$logging$threshold)
  logger
}


logger_has_console <- function(logger) {
  "console" %in% names(logger$appenders) ||
    "console" %in% names(logger$inherited_appenders)
}


logger_configure <- function(logger, console, threshold) {
  has_console <- logger_has_console(logger)
  if (console != has_console) {
    if (has_console) {
      if ("console" %in% names(logger$appenders)) {
        logger$remove_appender("console")
      }
      ## Technically this is too big a hammer, because it prevents any
      ## forwarding to the parent logger. However, it works fine with
      ## the way that we do use things because the parent logger only
      ## contains at most one logger, which *is* the console logger.
      ##
      ## lgr does not seem to have a sensible way of forwarding to
      ## only some of the inherited appenders, so if this is not
      ## enough we'll need to do inheritance slightly differently
      ## (e.g., copy appenders out of the parent into the child and
      ## not propagate).
      if ("console" %in% names(logger$inherited_appenders)) {
        logger$set_propagate(FALSE)
      }
    } else {
      logger$add_appender(outpack_console_appender(), name = "console")
    }
  }
  logger$set_threshold(threshold)
}


new_packet_logger <- function(path, root, id, console, threshold) {
  console <- console %||% root$config$logging$console
  threshold <- threshold %||% root$config$logging$threshold
  assert_scalar_logical(console, "logging_console")
  stopifnot(fs::is_dir(path))
  logger <- lgr::get_logger(c("outpack", root$id, id))
  file_json <- tempfile("outpack-log-", fileext = ".json")
  logger$add_appender(lgr::AppenderJson$new(file_json), name = "json")
  logger_configure(logger, console, threshold)
  logger
}


## We might tweak this later
read_log_json <- function(path, ...) {
  lgr::read_json_lines(path, ...)
}
