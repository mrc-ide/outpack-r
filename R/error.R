not_found_error <- function(message, data) {
  structure(list(message = message, data = data),
            class = c("not_found_error", "error", "condition"))
}
