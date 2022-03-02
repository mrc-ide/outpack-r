outpack_id <- function(time = Sys.time()) {
  ## TODO: there's a question here of if we should use UTC for the id
   sprintf("%s-%s%s",
          iso_time_str(time),
          val_to_bytes(as.numeric(time), 2),
          paste(as.character(openssl::rand_bytes(2)), collapse = ""))
}
