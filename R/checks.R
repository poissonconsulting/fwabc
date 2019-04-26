is_stream_wskey <- function(x) x %in% unique(fwa_lookup_stream$WATERSHED_KEY)
is_stream_wsgcode <- function(x) x %in% unique(fwa_lookup_stream$WATERSHED_GROUP_CODE)
is_stream_gnis <- function(x) x %in% unique(lookup_stream_gnis$GNIS_NAME)

is_wshed_wskey <- function(x) x %in% unique(fwa_lookup_watershed$WATERSHED_KEY)
is_wshed_wsgcode <- function(x) x %in% unique(fwa_lookup_watershed$WATERSHED_GROUP_CODE)
is_wshed_gnis <- function(x) x %in% unique(lookup_wshed_gnis$GNIS_NAME)

is_coast_wskey <- function(x) x %in% unique(fwa_lookup_coastline$WATERSHED_KEY)
is_coast_wsgcode <- function(x) x %in% unique(fwa_lookup_coastline$WATERSHED_GROUP_CODE)

is_wsgcode <- function(x) x %in% unique(fwa_lookup_watershedgroup$WATERSHED_GROUP_CODE)

check_tributaries <- function(x){
  checkor(check_logical(x), check_integer(x))
}

check_stream_gnis <- function(x){
  x <- x[!(is_stream_gnis(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid GNIS_NAME in the FWA_STREAM_NETWORKS_SP geodatabase.
           (see fwa_lookup_stream for reference)", conjunction = "and"))
}

check_wshed_gnis <- function(x){
  x <- x[!(is_wshed_gnis(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid GNIS_NAME in the FWA_WATERSHEDS_POLY geodatabase.
           (see fwa_lookup_watershed for reference)", conjunction = "and"))
}

check_stream <- function(x){
  x <- x[!(is_stream_wskey(x) | is_stream_wsgcode(x))]
  if(!length(x)) return(TRUE)
    err(co(x, some = "%c %r not valid WATERSHED_KEY or WATERSHED_GROUP_CODE
    in the FWA_STREAM_NETWORKS_SP geodatabase.
           (see fwa_lookup_stream for reference)", conjunction = "and"))
}

check_wshed <- function(x){
  x <- x[!(is_wshed_wskey(x) | is_wshed_wsgcode(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid WATERSHED_KEY or WATERSHED_GROUP_CODE
  in the FWA_WATERSHEDS_POLY geodatabase.
           (see fwa_lookup_watershed for reference)", conjunction = "and"))
}

check_coastline <- function(x){
  x <- x[!(is_coast_wskey(x) | is_coast_wsgcode(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid WATERSHED_KEY or WATERSHED_GROUP_CODE
  in the FWA_COASTLINE_SP layer of the FWA_BC geodatabase.
           (see fwa_lookup_coastline for reference)", conjunction = "and"))
}

check_wshedgroup <- function(x){
  x <- x[!(is_wsgcode(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid WATERSHED_GROUP_CODE in the FWA_WATERSHED_GROUPS_POLY layer of the FWA_BC geodatabase.
           (see fwa_lookup_watershedgroup for reference)", conjunction = "and"))
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
