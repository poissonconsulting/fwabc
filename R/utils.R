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
  d <- c(x, b[sapply(strsplit(c, "-"), function(x) length(x) <= n)])
  lookup_wskey$WATERSHED_KEY[lookup_wskey$FWA_WATERSHED_CODE %in% d]
}

### these are designed to also take as input what you are trying to convert to
wskey_to_wscode <- function(x){
  wscode <- x[is_wscode(x)]
  y <- lookup_wskey$FWA_WATERSHED_CODE[lookup_wskey$WATERSHED_KEY %in% x]
  unique(c(wscode, y))
}

all_data <- function(layer){
  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", layer)) %>%
    bcdata::collect()
}

filter_wskey <- function(x, layer, crs){
  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", layer), crs = crs) %>%
    bcdata::filter(WATERSHED_KEY %in% x) %>%
    bcdata::collect()
}

filter_wsgcode <- function(x, layer, crs){
  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", layer), crs = crs) %>%
    bcdata::filter(WATERSHED_GROUP_CODE %in% x) %>%
    bcdata::collect()
}

filter_both <- function(wskey, wsgcode, layer, crs){
  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", layer), crs = crs) %>%
    bcdata::filter(WATERSHED_GROUP_CODE %in% wsgcode | WATERSHED_KEY %in% wskey) %>%
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

