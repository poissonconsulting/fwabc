is_blk_stream <- function(x){
 x %in% fwa_lookup_stream_blkey$BlueLineKey
}

is_gnis <- function(x){
  x %in% fwa_lookup_stream_gnis$GnisName
}

is_ws_code_stream <- function(x){
  x %in% fwa_lookup_stream_blkey$WatershedCode
}

is_blk_coast <- function(x){
  x %in% fwa_lookup_coast_blkey$BlueLineKey
}

is_ws_code_coast <- function(x){
  x %in% fwa_lookup_coast_blkey$WatershedCode
}

is_ws_code_watershed <- function(x){
  x %in% fwa_lookup_watershed$WatershedCode
}

is_wsg_code <- function(x){
  x %in% fwa_lookup_wsgroup$WatershedGroupCode
}

is_wsg_name <- function(x){
  x %in% fwa_lookup_wsgroup$WatershedGroupName
}

is_wsg_code_coast <- function(x){
  x %in% fwa_lookup_coast_wsgroup$WatershedGroupCode
}

is_wsg_name_coast <- function(x){
  x %in% fwa_lookup_coast_wsgroup$WatershedGroupName
}

check_stream <- function(x){
  lapply(x, function(x){
    if(!(is_blk_stream(x) || is_gnis(x) || is_ws_code_stream(x))) err(x, " is not a valid GnisName, BlueLineKey, or WatershedCode (see fwa_lookup_stream_gnis and fwa_lookup_stream_blkey for reference)")
  })
}

check_watershed <- function(x){
  lapply(x, function(x){
    if(!(is_gnis(x) || is_ws_code_watershed(x))) err(x, " is not a valid GnisName, or WatershedCode (see fwa_lookup_stream_gnis and fwa_lookup_watershed and fwa_lookup_wsgroup for reference)")
  })
}

check_coastline <- function(x){
  lapply(x, function(x){
    if(!(is_blk_coast(x) || is_ws_code_coast(x) || is_wsg_code_coast(x) || is_wsg_name_coast(x))) err(x, " is not a valid coastline WatershedGroupName, WatershedGroupCode, BlueLineKey, or WatershedCode (see fwa_coastline_lookup for reference)")
  })
}

check_wsgroup <- function(x){
  lapply(x, function(x){
    if(!(is_wsg_code(x) || is_wsg_name(x))) err(x, " is not a valid WatershedGroupCode or WatershedGroupName (see fwa_lookup_wsgroup for reference)")
  })
}

check_dsn <- function(x, layer){
  check_string(x)
  works <- try(st_layers(dsn = x), silent = TRUE)
  if(inherits(works, "try-error")) err("Could not read any layers from database at ", x)
  if(!(layer %in% works$name)) err("Database at ", x, " does not have the the required layer: ", layer)
}

check_dsn <- function(x, layer){
  check_string(x)
  works <- try(st_layers(dsn = x), silent = TRUE)
  if(inherits(works, "try-error")) err("Could not read any layers from database at ", x)
  if(!(layer %in% works$name)) err("Database at ", x, " does not have the the required layer: ", layer)
}
