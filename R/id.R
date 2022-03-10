## Same basic approach as orderly
outpack_id <- function(time = Sys.time()) {
   sprintf("%s-%s%s",
          iso_time_str(time),
          val_to_bytes(as.numeric(time), 2),
          paste(as.character(openssl::rand_bytes(2)), collapse = ""))
}
