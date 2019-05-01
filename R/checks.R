check_tributaries <- function(x){
  checkor(check_logical(x), check_integer(x))
}

check_gnis <- function(x, layer){
  x <- x[!(is_gnis(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = "%c %r not valid GNIS_NAME
           (see fwa_lookup_gnis for reference)", conjunction = "and"))
}

check_wskey <- function(x, layer){
  x <- x[!(is_wskey(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid WATERSHED_KEY for layer '", layer, "'"), conjunction = "and"))
}

check_wsgcode <- function(x, layer){
  x <- x[!(is_wsgcode(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid WATERSHED_GROUP_CODE for layer '", layer, "'"), conjunction = "and"))
}

check_wsgname <- function(x, layer){
  x <- x[!(is_wsgname(x))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid WATERSHED_GROUP_NAME for layer '", layer, "'"), conjunction = "and"))
}

check_wskey_wsgcode <- function(x, layer){
  x <- x[!(is_wskey(x, layer) | is_wsgcode(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid WATERSHED_KEY or WATERSHED_GROUP_CODE for layer '", layer, "'"), conjunction = "and"))
}

check_layer <- function(x){
  if(x %in% fwa_lookup_layer) return(TRUE)
  err(x, " is not a valid layer (see fwa_lookup_layer for reference).")
}

# check_dsn <- function(x, layer){
#   check_string(x)
#   works <- try(st_layers(dsn = x), silent = TRUE)
#   if(inherits(works, "try-error"))
#     err("Could not read any layers from database at ", x)
#   if(!(layer %in% works$name))
#     err("Database at ", x, " does not have the the required layer: ", layer)
# }

# check_linestringz <- function(x){
#   works <- try(st_cast(x, "LINESTRING", silent = TRUE))
#   if(inherits(data, "try-error")) err("data cannot be cast to LINESTRING")
#   works <- try(st_coordinates(x)[,"Z"])
#   if(inherits(data, "try-error")) err("there is no Z (elevation) coordinate")
# }
