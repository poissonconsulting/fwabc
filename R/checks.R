is_blk_stream <- function(x){
  x %in% fwa_blue_line_key$BlueLineKey
}

is_blk_coast <- function(x){
  x %in% fwa_blk_coastline$BlueLineKey
}

is_gnis <- function(x){
  x %in% fwa_gnis$GnisName
}

is_wscode <- function(x){
  x %in% fwa_watershed_group$WatershedGroupCode
}

is_wsname <- function(x){
  x %in% fwa_watershed_group$WatershedGroupName
}

# is_shortcut <- function(x){
#   sc <- lapply(fwa_layers, function(x) x$shortcut) %>% unlist(., use.names = FALSE)
#   x %in% sc
# }

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
