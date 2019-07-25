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

check_x_layer <- function(x, what, types, layer){
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

# check_linestringz <- function(x){
#   works <- try(st_cast(x, "LINESTRING", silent = TRUE))
#   if(inherits(data, "try-error")) err("data cannot be cast to LINESTRING")
#   works <- try(st_coordinates(x)[,"Z"])
#   if(inherits(data, "try-error")) err("there is no Z (elevation) coordinate")
# }
