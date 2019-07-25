quiet <- function(x) suppressWarnings(suppressMessages(x))

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

what_is_it <- function(x){
  if(is_gnis(x))
    return("GNIS_NAME")
  if(is_wsgcode(x))
    return("WATERSHED_GROUP_CODE")
  if(is_wsgname(x))
    return("WATERSHED_GROUP_NAME")
  if(is_wskey(x))
    return("WATERSHED_KEY")
  if(is.null(x))
    return("NULL")
  FALSE
}

is_layer <- function(x){
  x %in% lookup_layer$layer || is.null(x)
}

### converter functions
wskey_to_wscode <- function(x, layer){
  look <- lookup_wskey[lookup_wskey[[layer]],]
  y <- look$FWA_WATERSHED_CODE[look$WATERSHED_KEY %in% x]
  unique(gsub("-000000", "", y))
}

gnis_to_wscode <- function(x){
  look <- lookup_gnis[lookup_gnis[[layer]],]
  y <- look$FWA_WATERSHED_CODE[look$GNIS_NAME %in% x]
  unique(gsub("-000000", "", y))
}

wsgcode_to_wsgname <- function(x){
  lookup_wsgroup$WATERSHED_GROUP_NAME[lookup_wsgroup$WATERSHED_GROUP_CODE %in% x]
}

wsgname_to_wsgcode <- function(x){
  lookup_wsgroup$WATERSHED_GROUP_CODE[lookup_wsgroup$WATERSHED_GROUP_NAME %in% x]
}

# line_sample <- function(x, distance){
#   x <- x %>% st_cast("LINESTRING")
#   sample <- seq(0, 1, 1/as.vector(round(st_length(x)/distance)))
#   x %>%
#     st_line_sample(sample = sample)  %>%
#     st_cast("POINT")
# }
#
# line_rkm <- function(x, distance, label_name = "Rkm", sfc_name = "geometry"){
#   # check_linestringz(x)
#
#   start <- x %>% st_cast("POINT")
#   start <- start[which.min(st_coordinates(start)[,"Z"]),]
#
#   pts <- line_sample(x, distance)
#   i <- st_nearest_feature(start %>% st_zm(), pts %>% st_zm())
#   n <- length(pts)
#   label <- seq(0, n*distance, distance)/1000
#   if(i > n/2){
#     label <- rev(label)
#   }
#   dat <- data.frame(label[1:n], pts)
#   names(dat) <- c(label_name, sfc_name)
#   dat %>% st_sf()
# }

