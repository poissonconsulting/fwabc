is_wskey <- function(x, layer = NULL){
  if(is.null(layer)){
    return(x %in% unique(lookup_wskey$WATERSHED_KEY))
  }
  x %in% unique(lookup_wskey$WATERSHED_KEY[lookup_wskey[[layer]]])
}

is_wscode <- function(x, layer = NULL){
  if(is.null(layer)){
    return(x %in% unique(lookup_wskey$FWA_WATERSHED_CODE))
  }
  x %in% unique(lookup_wskey$FWA_WATERSHED_CODE[lookup_wskey[[layer]]])
}

is_wsgcode <- function(x, layer = NULL){
  if(is.null(layer)){
    return(x %in% unique(lookup_wsgroup$WATERSHED_GROUP_CODE))
  }
  x %in% unique(lookup_wsgroup$WATERSHED_GROUP_CODE[lookup_wsgroup[[layer]]])
}

is_wsgname <- function(x, layer = NULL){
  if(is.null(layer)){
    return(x %in% unique(lookup_wsgroup$WATERSHED_GROUP_NAME))
  }
  x %in% unique(lookup_wsgroup$WATERSHED_GROUP_NAME[lookup_wsgroup[[layer]]])
}

is_gnis <- function(x, layer = NULL){
  if(is.null(layer)){
    return(x %in% unique(lookup_gnis$GNIS_NAME))
  }
  x %in% unique(lookup_gnis$GNIS_NAME[lookup_gnis[[layer]]])
}

tribs <- function(x, n){
  x <- as.character(x)
  a <- gsub("-000000", "", x)
  b <- lookup_wskey$FWA_WATERSHED_CODE[grepl(a, lookup_wskey$FWA_WATERSHED_CODE, fixed = TRUE)]
  c <- gsub("-000000", "", b) %>% gsub(paste0(a, "-"), "", .)
  d <- c(x, b[sapply(strsplit(c, "-"), function(x) length(x) <= order)])
  lookup_wskey$WATERSHED_KEY[lookup_wskey$FWA_WATERSHED_CODE %in% d]
}

wskey_to_wscode <- function(x){
  lookup_wskey$FWA_WATERSHED_CODE[lookup_wskey$WATERSHED_KEY %in% x]
}

wsgcode_to_wskey <- function(x){
  wskey <- x[is_wskey(x)]
  y <- lookup_wskey$WATERSHED_KEY[lookup_wskey$WATERSHED_GROUP_CODE %in% x]
  unique(as.numeric(c(wskey, y)))
}

wskey_to_wsgcode <- function(x){
  wsgcode <- x[is_wsgcode(x)]
  y <- lookup_wskey$WATERSHED_GROUP_CODE[lookup_wskey$WATERSHED_KEY %in% x]
  unique(c(wsgcode, y))
}

all_data <- function(layer){
  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", layer)) %>%
    bcdata::collect()
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

