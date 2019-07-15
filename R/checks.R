check_x <- function(x, types){
  if(types == "gkcn"){
    if(!(all(is_gnis(x)) || all(is_wsgcode(x)) ||
      all(is_wsgname(x)) || all(is_wskey(x))))
      err("x must be a vector of valid GNIS_NAME, WATERSHED_KEY,
      WATERSHED_GROUP_CODE, or WATERSHED_GROUP_NAME")
  }

  if(types == "kcn"){
    if(!(all(is_wsgcode(x)) ||
         all(is_wsgname(x)) || all(is_wskey(x))))
      err("x must be a vector of valid WATERSHED_KEY,
      WATERSHED_GROUP_CODE, or WATERSHED_GROUP_NAME")
  }

  if(types == "cn"){
    if(!(all(is_wsgname(x)) || all(is_wskey(x))))
      err("x must be a vector of WATERSHED_GROUP_CODE or WATERSHED_GROUP_NAME")
  }

  if(types == "gk"){
    if(!(all(is_gnis(x)) || all(is_wsgcode(x))))
      err("x must be a vector of valid GNIS_NAME or WATERSHED_KEY")
  }
  x
}

check_x_layer <- function(x, types, layer){
  what <- what_is_it(x[1])
  if(types == "gkcn"){
    y <- switch(what,
                "GNIS_NAME" = x[!is_gnis(x, layer)],
                "WATERSHED_GROUP_CODE" = x[!is_wsgcode(x, layer)],
                "WATERSHED_GROUP_NAME" = x[!is_wsgname(x, layer)],
                "WATERSHED_KEY" = x[!is_wskey(x, layer)])
  }

  if(types == "kcn"){
    y <- switch(what,
                "WATERSHED_GROUP_CODE" = x[!is_wsgcode(x, layer)],
                "WATERSHED_GROUP_NAME" = x[!is_wsgname(x, layer)],
                "WATERSHED_KEY" = x[!is_wskey(x, layer)])
  }

  if(types == "cn"){
    y <- switch(what,
                "WATERSHED_GROUP_CODE" = x[!is_wsgcode(x, layer)],
                "WATERSHED_GROUP_NAME" = x[!is_wsgname(x, layer)])
  }

  if(types == "gk"){
    y <- switch(what,
                "GNIS_NAME" = x[!is_gnis(x, layer)],
                "WATERSHED_KEY" = x[!is_wskey(x, layer)])
  }
  if(length(y) > 0)
    err(co(x, some = paste0("%c ", what, "%r not available for layer '", layer, "'"),
           one = paste0("%c ", what, " is not available for layer '", layer, "'"),  conjunction = "and"))
  x
}

check_tributaries <- function(x){
  checkor(check_logical(x), check_integer(x))
}

check_gnis <- function(x, layer = NULL){
  x <- x[!(is_gnis(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid GNIS_NAME for layer '", layer, "'"), conjunction = "and"))
}

check_wskey <- function(x, layer = NULL){
  x <- x[!(is_wskey(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid WATERSHED_KEY for layer '", layer, "'"), conjunction = "and"))
}

check_wsgcode <- function(x, layer = NULL){
  x <- x[!(is_wsgcode(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid WATERSHED_GROUP_CODE for layer '", layer, "'"), conjunction = "and"))
}

check_wsgname <- function(x, layer = NULL){
  x <- x[!(is_wsgname(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid WATERSHED_GROUP_NAME for layer '", layer, "'"), conjunction = "and"))
}

check_wskey_wsgcode <- function(x, layer = NULL){
  x <- x[!(is_wskey(x, layer) | is_wsgcode(x, layer))]
  if(!length(x)) return(TRUE)
  err(co(x, some = paste0("%c %r not valid WATERSHED_KEY or WATERSHED_GROUP_CODE for layer '", layer, "'"), conjunction = "and"))
}

check_layer <- function(x){
  checkor(check_length(x, 1L), check_null(x))
  if(x %in% lookup_layer$layer || is.null(x)) return(TRUE)
  err(x, " is not a valid layer (see fwa_lookup_layer for reference).")
}

check_layers <- function(x){
  x <- x[!(is_layer(x))]
  if(!length(x)) return(TRUE)
  err(x, " is not a valid layer (see fwa_lookup_layer for reference).")
}

# check_linestringz <- function(x){
#   works <- try(st_cast(x, "LINESTRING", silent = TRUE))
#   if(inherits(data, "try-error")) err("data cannot be cast to LINESTRING")
#   works <- try(st_coordinates(x)[,"Z"])
#   if(inherits(data, "try-error")) err("there is no Z (elevation) coordinate")
# }
