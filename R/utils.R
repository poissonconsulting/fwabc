stream_to_blk <- function(x){
  unlist(lapply(x, function(x){
    if(is_blk_stream(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(fwa_gnis_lookup$BlueLineKey[fwa_gnis_lookup$GnisName %in% x])
    }
    fwa_stream_lookup$BlueLineKey[fwa_stream_lookup$WatershedCode %in% x]
  }))
}

stream_to_wscode <- function(x){
  unlist(lapply(x, function(x){
    if(is_ws_code_stream(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(fwa_gnis_lookup$WatershedCode[fwa_gnis_lookup$GnisName %in% x])
    }
    fwa_stream_lookup$WatershedCode[fwa_stream_lookup$BlueLineKey %in% x]
  }))
}

tribs <- function(x){
  x <- gsub("-000000", "", x)
  unlist(lapply(x, function(x){
    fwa_stream_lookup$BlueLineKey[grepl(x, fwa_stream_lookup$WatershedCode, fixed = TRUE)]
  }))
}

wsgname_to_wsgcode <- function(x){
  checkor(check_ws(x), is.null(x))
  unlist(lapply(x, function(x){
    if(is_wsg_code(x)){
      return(x)
    }
    fwa_wsgroup_lookup$WatershedGroupCode[fwa_wsgroup_lookup$WatershedGroupName %in% x]
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

