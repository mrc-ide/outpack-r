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
    layout = outpack_console_layout$new())
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
  logger
}


new_root_logger <- function(id, config) {
  logger <- lgr::get_logger(c("outpack", id))
  logger_configure(logger, config$logging$console, config$logging$threshold)
  logger
}


logger_configure <- function(logger, console, threshold) {
  appenders <- c(names(logger$appenders), names(logger$inherited_appenders))
  has_console_logger <- "console" %in% appenders
  if (console != has_console_logger) {
    if (has_console_logger) {
      logger$remove_appender("console")
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
