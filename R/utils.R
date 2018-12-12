stream_to_blk <- function(x){
  unlist(lapply(x, function(x){
    if(is_blk_s(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(fwa_gnis_lookup$BlueLineKey[which(fwa_gnis$GnisName %in% x)])
    }
    fwa_stream_lookup$BlueLineKey[which(fwa_stream_lookup$WatershedCode %in% x)]
  }))
}

stream_to_wscode <- function(x){
  unlist(lapply(x, function(x){
    if(is_wscode_s(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(fwa_gnis_lookup$WatershedCode[fwa_gnis_lookup$GnisName %in% x])
    }
    fwa_stream_lookup$WatershedCode[fwa_stream_lookup$BlueLineKey %in% x]
  }))
}

wscode_to_blk <- function(x){
  unlist(lapply(x, function(x){
    fwa_stream_lookup$BlueLineKey[which(fwa_stream_lookup$WatershedCode %in% x)]
  }))
}

fwa_tributaries <- function(x){
  x <- gsub("-000000", "", x)
  unlist(lapply(x, function(x){
    fwa_stream_lookup$BlueLineKey[grepl(x, fwa_stream_lookup$WatershedCode, fixed = TRUE)]
  }))
}

# stream_to_blk(fwa_stream_lookup$WatershedCode[1:5])

gnis_to_blk <- function(x){
  check_stream(x)
  unlist(lapply(x, function(x){
    if(is_blk_stream(x)){
      return(x)
    }
    fwa_gnis$BlueLineKey[which(fwa_gnis$GnisName %in% x)]
  }))
}

wsname_to_wscode <- function(x){
  checkor(check_ws(x), is.null(x))
  unlist(lapply(x, function(x){
    if(is_wscode(x)){
      return(x)
    }
    fwa_watershed_group$WatershedGroupCode[which(fwa_watershed_group$WatershedGroupName %in% x)]
  }))
}

line_sample <- function(data, distance){
  # data <- try(st_cast(data, "LINESTRING", silent = TRUE))
  # if(inherits(data, "try-error")) ps_error("data cannot be cast to LINESTRING")
  sample <- seq(0, 1, 1/as.vector(round(st_length(data)/distance)))

  data %>%
    st_line_sample(sample = sample)  %>%
    st_cast("POINT") %>%
    st_sf()
}

