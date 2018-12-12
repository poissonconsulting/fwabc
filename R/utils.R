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

or_sql <- function(x, var = "BLUE_LINE_KEY"){
  paste0(var, " = ", x, " OR ", collapse = "")
}

select_sql <- function(x, vars = "*", layer = "FWA_ROUTES_SP"){
  paste("select", vars, "from", layer, "where", x)
}

snip <- function(x){
  gsub('.{0,4}$', '', x)
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

# shortcut_to_layer <- function(x){
#   if(is_shortcut(x)){
#     data <- do.call("rbind", lapply(fwa_layers, function(x) x))
#     return(data$layer[which(data$shortcut == x)])
#   }
#   x
# }
