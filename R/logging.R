outpack_log <- function(object, log_level, topic, detail, caller) {
  if (inherits(object, "outpack_packet") || inherits(object, "outpack_root")) {
    logger <- object$logger
  } else {
    stop("Invalid call to outpack_log")
  }

  assert_scalar_character(topic)
  assert_scalar_character(caller)
  assert_character(detail)

  if (log_show(log_level, logger)) {
    if (logger$console) {
      log_console(topic, detail, caller, log_level)
    }
    if (!is.null(logger$json)) {
      logger$json$append(topic, detail, caller, log_level)
    }
  }
}


outpack_log_info <- function(object, topic, detail, caller) {
  outpack_log(object, "info", topic, detail, caller)
}


outpack_log_debug <- function(object, topic, detail, caller) {
  outpack_log(object, "debug", topic, detail, caller)
}


outpack_packet_logger <- function(path, root, console, threshold) {
  ret <- root$config$logging
  ret$json <- log_collector_json()

  ## Override root defaults with arguments:
  if (!is.null(console)) {
    ret$console <- assert_scalar_logical(console, "logging_console")
  }
  if (!is.null(threshold)) {
    ret$threshold <- log_level_check(threshold)
  }

  ret
}


log_console <- function(topic, detail, caller, log_level) {
  ## Filter out some log types; these will never want echoing to the
  ## console like via this function and I imagine that this list will
  ## grow...
  if (caller == "outpack::outpack_packet_run" && topic == "output") {
    return()
  }
  if (length(detail) > 1) {
    topic <- c(topic, rep_len("...", length(detail) - 1))
  }
  ## This is the original orderly log format, seems like a sensible
  ## one to use here, at least for now, as users are familar with it.
  ## We'll sort out colouring here later, and/or possibly use cli for
  ## the final format. For now though we just care about getting
  ## things out.
  str <- trimws(sprintf("[ %s ]  %s", format(topic, width = 10), detail))
  message(paste(str, collapse = "\n"))
}


log_collector_json <- function() {
  env <- new.env(parent = emptyenv())
  env$data <- list()
  list(
    append = function(topic, detail, caller, log_level) {
      el <- list(topic = topic,
                 detail = detail,
                 caller = caller,
                 log_level = log_level,
                 time = as.numeric(Sys.time()))
      env$data <- c(env$data, list(el))
    },
    get = function() {
      log_serialise(env$data)
    }
  )
}


log_serialise <- function(data) {
  log_serialise_entry <- function(el) {
    if (length(el$detail) != 1) {
      el$detail <- I(el$detail)
    }
    el
  }
  data <- lapply(data, log_serialise_entry)
  to_json(data, "log", auto_unbox = TRUE, digits = NA)
}


log_read <- function(path) {
  d <- from_json(path, simplifyDataFrame = TRUE)
  if (is.character(d$detail)) {
    d$detail <- I(as.list(d$detail))
  }
  d$time <- num_to_time(d$time)
  d
}


log_levels <- c("info", "debug", "trace")


log_level_check <- function(level, name = deparse(substitute(level))) {
  match_value(level, log_levels, name)
}


log_show <- function(log_level, logger) {
  match(log_level_check(log_level), log_levels) <=
    match(logger$threshold, log_levels)
}
