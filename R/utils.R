is_wskey <- function(x) x %in% unique(lookup_wskey$WATERSHED_KEY)
is_wscode <- function(x) x %in% unique(lookup_wskey$FWA_WATERSHED_CODE)
is_wsgcode <- function(x) x %in% unique(lookup_wsgroup$WATERSHED_GROUP_CODE)
is_wsgname <- function(x) x %in% unique(lookup_wsgroup$WATERSHED_GROUP_NAME)
is_gnis <- function(x) x %in% unique(lookup_gnis$GNIS_NAME)

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

