is_blk_stream <- function(x) x %in% unique(fwa_lookup_stream$BLUE_LINE_KEY)

is_gnis <- function(x) x %in% unique(fwa_lookup_stream$GNIS_NAME)

is_ws_code_stream <- function(x) x %in% fwa_lookup_stream$FWA_WATERSHED_CODE

is_blk_coast <- function(x) x %in% fwa_lookup_coastline$BLUE_LINE_KEY

is_ws_code_coast <- function(x) x %in% fwa_lookup_coastline$FWA_WATERSHED_CODE

is_wsg_code_coast <- function(x) x %in% fwa_lookup_coastline$WATERSHED_GROUP_CODE

is_wsg_name_coast <- function(x) x %in% fwa_lookup_watershedgroup$WATERSHED_GROUP_NAME[fwa_lookup_watershedgroup$WATERSHED_GROUP_CODE %in%
                                                                                         fwa_lookup_coastline$WATERSHED_GROUP_CODE]
is_ws_code_watershed <- function(x) x %in% fwa_lookup_watershed$FWA_WATERSHED_CODE

is_wsg_code <- function(x) x %in% fwa_lookup_watershedgroup$WATERSHED_GROUP_CODE

is_wsg_name <- function(x) x %in% fwa_lookup_watershedgroup$WATERSHED_GROUP_NAME

check_tributaries <- function(x){
  checkor(check_logical(x), check_integer(x))
}

check_stream <- function(x){
  x <- x[!(is_blk_stream(x) | is_gnis(x) | is_ws_code_stream(x))]
  if(!length(x)) return(TRUE)
    err(co(x, some = "%c %r not valid GNIS_NAME, BLUE_LINE_KEY or FWA_WATERSHED_CODE
           (see fwa_lookup_stream for reference or use fwa_search_gnis
           function to match a regular expression to valid GNIS_NAME)", conjunction = "and"))
}

check_watershed <- function(x){
  x <- x[!(is_gnis(x) | is_ws_code_watershed(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid GNIS_NAME or FWA_WATERSHED_CODE
           (see fwa_lookup_stream and fwa_lookup_watershed for reference)", conjunction = "and"))
}

check_coastline <- function(x){
  x <- x[!(is_blk_coast(x) | is_ws_code_coast(x) | is_wsg_code_coast(x) | is_wsg_name_coast(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid WATERSHED_GROUP_NAME
           (see see fwa_coastline_lookup for reference)", conjunction = "and"))
}

check_watershedgroup <- function(x){
  x <- x[!(is_wsg_code(x) || is_wsg_name(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid WATERSHED_GROUP_CODE or WATERSHED_GROUP_NAME
           (see fwa_lookup_wsgroup for reference or use fwa_search_watershedgroup
         function to match a regular expression to valid WATERSHED_GROUP_NAME)", conjunction = "and"))
}

check_dsn <- function(x, layer){
  check_string(x)
  works <- try(st_layers(dsn = x), silent = TRUE)
  if(inherits(works, "try-error"))
    err("Could not read any layers from database at ", x)
  if(!(layer %in% works$name))
    err("Database at ", x, " does not have the the required layer: ", layer)
}

# check_linestringz <- function(x){
#   works <- try(st_cast(x, "LINESTRING", silent = TRUE))
#   if(inherits(data, "try-error")) err("data cannot be cast to LINESTRING")
#   works <- try(st_coordinates(x)[,"Z"])
#   if(inherits(data, "try-error")) err("there is no Z (elevation) coordinate")
# }
