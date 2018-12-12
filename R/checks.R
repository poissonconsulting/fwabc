is_blk_s <- function(x){
 x %in% fwa_stream_lookup$BlueLineKey
}

is_gnis <- function(x){
  x %in% fwa_gnis_lookup$GnisName
}

is_wscode_s <- function(x){
  x %in% fwa_stream_lookup$WatershedCode
}

is_blk_c <- function(x){
  x %in% fwa_coastline_lookup$BlueLineKey
}

is_wscode_c <- function(x){
  x %in% fwa_coastline_lookup$WatershedCode
}

is_wsg_code <- function(x){
  x %in% fwa_wsgroup_lookup$WatershedGroupCode
}

is_wsg_name <- function(x){
  x %in% fwa_wsgroup_lookup$WatershedGroupName
}

is_wsg_code_c <- function(x){
  x %in% fwa_coastline_lookup$WatershedGroupCode
}

is_wsg_name_c <- function(x){
  x %in% fwa_coastline_lookup$WatershedGroupName
}

check_stream <- function(x){
  lapply(x, function(x){
    if(!(is_blk_s(x) || is_gnis(x) || is_wscode_s(x))) err(x, " is not a valid GnisName, BlueLineKey, or WatershedCode (see fwa_stream_lookup for reference)")
  })
}

check_coastline <- function(x){
  lapply(x, function(x){
    if(!(is_blk_c(x) || is_wscode_c(x) || is_wsg_code_c(x) || is_wsg_name_c(x))) err(x, " is not a valid coastline WatershedGroupName, WatershedGroupCode, BlueLineKey, or WatershedCode (see fwa_coastline_lookup for reference)")
  })
}

check_wsgroup <- function(x){
  lapply(x, function(x){
    if(!(is_wsg_code(x) || is_wsg_name(x))) err(x, " is not a valid WatershedGroupCode or WatershedGroupName")
  })
}

check_dsn <- function(x, layer){
  checkr::check_string(x)
  works <- try(st_layers(dsn = x), silent = TRUE)
  if(inherits(works, "try-error")) err("Could not read any layers from database at ", x)
  if(!(layer %in% works$name)) err("Database at ", x, " does not have the the required layer: ", layer)
}
