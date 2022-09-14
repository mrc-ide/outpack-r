check_device_stack <- function(expected) {
  check <- length(grDevices::dev.list()) - expected
  if (check == 0) {
    return()
  } else if (check > 0) {
    for (i in seq_len(check)) {
      grDevices::dev.off()
    }
    stop(ngettext(check,
                  "Script left 1 device open",
                  sprintf("Script left %d devices open", check)))
  } else {
    stop(sprintf("Script closed %d more devices than it opened!", abs(check)))
  }
}


check_sink_stack <- function(expected) {
  check <- sink.number() - expected
  if (check == 0) {
    return()
  } else if (check > 0) {
    for (i in seq_len(check)) {
      sink(NULL) # nolint
    }
    stop(ngettext(check,
                  "Script left 1 sink open",
                  sprintf("Script left %d sinks open", check)))
  } else {
    stop(sprintf("Script closed %d more sinks than it opened!", abs(check)))
  }
}
