local <- "local"
orphan <- "orphan"
location_reserved_name <- c(local, orphan)
location_types <- c(local, orphan, "path", "http")
## NOTE: could read this from id.json
re_id <- "^([0-9]{8}-[0-9]{6}-[[:xdigit:]]{8})$"
