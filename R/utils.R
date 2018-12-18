stream_to_blk <- function(x){
  unlist(lapply(x, function(x){
    if(is_blk_stream(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(fwa_lookup_stream_gnis$BlueLineKey[fwa_lookup_stream_gnis$GnisName %in% x])
    }
    fwa_lookup_stream_blkey$BlueLineKey[fwa_lookup_stream_blkey$WatershedCode %in% x]
  }))
}

stream_to_wscode <- function(x){
  unlist(lapply(x, function(x){
    if(is_ws_code_stream(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(fwa_lookup_stream_gnis$WatershedCode[fwa_lookup_stream_gnis$GnisName %in% x])
    }
    fwa_lookup_stream_blkey$WatershedCode[fwa_lookup_stream_blkey$BlueLineKey %in% x]
  }))
}

match_gnis <- function(x, ...){
  unlist(lapply(x, function(x){
    if(is_gnis(x) || is_blk_stream(x) || is_ws_code_stream(x)){
      return(x)
    }
    y <- fwa_search_gnis(x, ...)
    if(!length(y)){
      msg(x, " does not match any GnisName")
    } else {
      msg(x, " matched with GnisNames: ", y)
    }
    y
  }))
}

match_wsgroup <- function(x, ...){
  unlist(lapply(x, function(x){
    if(is_wsg_code(x) || is_wsg_name(x)){
      return(x)
    }
    y <- fwa_search_wsgroup(x, ...)
    if(!length(y)){
      msg(x, " does not match any WatershedGroupName")
    } else {
      msg(x, " matched with WatershedGroupName: ", y)
    }
    y
  }))
}

watershed_to_wscode <- function(x, group){
  unlist(lapply(x, function(x){
    y <- stream_to_wscode(x)
    code <- fwa_lookup_watershed$WatershedCode[fwa_lookup_watershed$WatershedCode %in% y & fwa_lookup_watershed$WatershedGroupCode == group]
    if(!length(code)) err(x, " is not in watershed group: ", group)
    code
  }))
}

wsgname_to_wsgcode <- function(x){
  unlist(lapply(x, function(x){
    if(!(is_wsg_name(x))){
      return(x)
    }
    fwa_lookup_wsgroup$WatershedGroupCode[fwa_lookup_wsgroup$WatershedGroupName %in% x]
  }))
}

tribs_stream <- function(x){
  x <- gsub("-000000", "", x)
  unlist(lapply(x, function(x){
    fwa_lookup_stream_blkey$BlueLineKey[grepl(x, fwa_lookup_stream_blkey$WatershedCode, fixed = TRUE)]
  }))
}

tribs_wshed <- function(x){
  x <- gsub("-000000", "", x)
  unlist(lapply(x, function(x){
    fwa_lookup_watershed$WatershedCode[grepl(x, fwa_lookup_watershed$WatershedCode, fixed = TRUE)]
  }))
}

line_sample <- function(x, distance){
  x <- x %>% st_cast("LINESTRING")
  sample <- seq(0, 1, 1/as.vector(round(st_length(x)/distance)))
  x %>%
    st_line_sample(sample = sample)  %>%
    st_cast("POINT")
}

line_rkm <- function(x, distance, label_name = "Rkm", sfc_name = "geometry"){
  # check_linestringz(x)

  start <- x %>% st_cast("POINT")
  start <- start[which.min(st_coordinates(start)[,"Z"]),]

  pts <- line_sample(x, distance)
  i <- st_nearest_feature(start %>% st_zm(), pts %>% st_zm())
  n <- length(pts)
  label <- seq(0, n*distance, distance)/1000
  if(i > n/2){
    label <- rev(label)
  }
  data.frame(label[1:n], pts) %>%
    setNames(c(label_name, sfc_name)) %>%
    st_sf()
}

