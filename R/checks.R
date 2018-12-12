is_blks <- function(x){
  x %in% fwa_stream_lookup$BlueLineKey
}

is_gnis <- function(x){
  x %in% fwa_gnis_lookup$GnisName
}

is_wscode <- function(x){
  x %in% fwa_stream_lookup$WatershedCode
}

is_blk_coast <- function(x){
  x %in% fwa_blk_coastline$BlueLineKey
}

is_wsg_code <- function(x){
  x %in% fwa_wsgroup_lookup$WatershedGroupCode
}

is_wsg_name <- function(x){
  x %in% fwa_wsgroup_lookup$WatershedGroupName
}

check_stream <- function(x){
  lapply(x, function(x){
    if(!(is_blk_stream(x) || is_gnis(x))){ps_error(x, " is not a valid BlueLineKey or GnisName")}
  })
}

check_blk_coast <- function(x){
  lapply(x, function(x){
    if(!(is_blk_coast(x))){ps_error(x, " is not a valid BlueLineKey")}
  })
}

check_ws <- function(x){
  lapply(x, function(x){
    if(!(is_wscode(x) || is_wsname(x))) ps_error(x, " is not a valid WatershedGroupCode or WatershedGroupName")
  })
}

check_dsn <- function(x, layer){
  check_string(x)
  works <- try(st_layers(dsn = x), silent = TRUE)
  if(inherits(works, "try-error")) ps_error("Could not read any layers from database at ", x)
  if(!(layer %in% works$name)){ps_error("Database at ", x, " does not have the the required layer: ", layer)}
}

check_layer <- function(x){
  if(!(x %in% unlist(fwa_layers, use.names = FALSE))){
    ps_error(x, " is not a valid layer or shortcut")
  }
}
