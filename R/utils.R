stream_to_blk <- function(x){
  unlist(lapply(x, function(x){
    if(is_blk_stream(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(lookup_stream_gnis$BLUE_LINE_KEY[which(lookup_stream_gnis$GNIS_NAME == x)])
    }
    fwa_lookup_stream$BLUE_LINE_KEY[fwa_lookup_stream$FWA_WATERSHED_CODE %in% x]
  }))
}

stream_to_wscode <- function(x){
  unlist(lapply(x, function(x){
    if(is_ws_code_stream(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(lookup_stream_gnis$FWA_WATERSHED_CODE[lookup_stream_gnis$GNIS_NAME == x])
    }
    fwa_lookup_stream$FWA_WATERSHED_CODE[fwa_lookup_stream$BLUE_LINE_KEY == x]
  }))
}

watershed_to_wscode <- function(x, group){
  unlist(lapply(x, function(x){
    y <- stream_to_wscode(x)
    code <- fwa_lookup_watershed$FWA_WATERSHED_CODE[fwa_lookup_watershed$FWA_WATERSHED_CODE %in% y & fwa_lookup_watershed$FWA_WATERSHED_CODE == group]
    if(!length(code)) err(x, " is not in watershed group: ", group)
    code
  }))
}

wsgname_to_wsgcode <- function(x){
  unlist(lapply(x, function(x){
    if(!(is_wsg_name(x))){
      return(x)
    }
    fwa_lookup_watershedgroup$WATERSHED_GROUP_CODE[fwa_lookup_watershedgroup$WATERSHED_GROUP_NAME == x]
  }))
}

tribs_streams <- function(x, n){
  unlist(lapply(x, function(x){tribs_stream(x, n)}))
}

tribs_stream <- function(x, n){
  x <- as.character(x)
  a <- gsub("-000000", "", x)
  b <- fwa_lookup_stream_blkey$WatershedCode[grepl(a, fwa_lookup_stream_blkey$WatershedCode, fixed = TRUE)]
  c <- gsub("-000000", "", b) %>% gsub(paste0(a, "-"), "", .)
  d <- c(x, b[sapply(strsplit(c, "-"), function(x) length(x) <= n)])
  fwa_lookup_stream_blkey$BlueLineKey[fwa_lookup_stream_blkey$WatershedCode %in% d]
}

tribs_wsheds <- function(x, n){
  unlist(lapply(x, function(x){tribs_wshed(x, n)}))
}

tribs_wshed <- function(x, n){
  x <- as.character(x)
  a <- gsub("-000000", "", x)
  b <- fwa_lookup_watershed$WatershedCode[grepl(a, fwa_lookup_watershed$WatershedCode, fixed = TRUE)]
  c <- gsub("-000000", "", b) %>% gsub(paste0(a, "-"), "", .)
  c(x, b[sapply(strsplit(c, "-"), function(x) length(x) <= n)])
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

