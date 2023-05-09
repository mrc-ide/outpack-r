check_device_stack <- function(expected) {
  check <- length(grDevices::dev.list()) - expected
  success <- check == 0
  if (check == 0) {
    msg <- NULL
  } else if (check > 0) {
    for (i in seq_len(check)) {
      grDevices::dev.off()
    }
    msg <- ngettext(check,
                    "Script left 1 device open",
                    sprintf("Script left %d devices open", check))
  } else {
    msg <- sprintf("Script closed %d more devices than it opened!", abs(check))
  }
  list(success = success, message = msg)
}


check_sink_stack <- function(expected) {
  check <- sink.number() - expected
  success <- check == 0
  if (check == 0) {
    msg <- NULL
  } else if (check > 0) {
    for (i in seq_len(check)) {
      sink(NULL) # nolint
    }
    msg <- ngettext(check,
                    "Script left 1 sink open",
                    sprintf("Script left %d sinks open", check))
  } else {
    msg <- sprintf("Script closed %d more sinks than it opened!", abs(check))
  }
  list(success = success, message = msg)
}
