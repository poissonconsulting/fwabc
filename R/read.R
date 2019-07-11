#' Read features from FWA database layer.
#'
#' @param x A vector of valid WATERSHED_KEY or WATERSHED_GROUP_CODE. If NULL, entire dataset is read.
#' @param layer A character string of valid layer name. See fwa_lookup_layer.
#' @param crs The epsg code for the coordinate reference system. Defaults to `3005`
#'        (B.C. Albers). See https://epsgi.io.
#' @param ask A flag indicating whether to ask before reading entire dataset.
#' @return A sf object.
#' @examples
#'\dontrun{
#' fwa_read_layer(c("VICT", 360843586), layer = "stream-network")
#' }
#' @export
fwa_read_layer <- function(x = NULL, layer = "stream-network", crs = 3005, ask = TRUE) {

  check_layer(layer)

  if(is.null(x)){
    if(!ask){
      return(all_data(layer))
    }
    if(yesno::yesno("This is a very large dataset. Do you want to download the entire ",
                    layer, " layer?")){
      return(all_data(layer))
    }
    return()
  }

  if(layer %in% lookup_layer$layer[!lookup_layer$WATERSHED_KEY]){
    check_wsgcode(x)
    return(filter_wsgcode(x, layer = layer, crs = crs))
  }

  check_wskey_wsgcode(x, layer = layer)
  wskey <- x[is_wskey(x)]
  wsgcode <- x[is_wsgcode(x)]

  if(length(wskey) & !length(wsgcode))
    return(filter_wskey(x, layer = layer, crs = crs))

  if(length(wsgcode) & !length(wskey))
    return(filter_wsgcode(x, layer = layer, crs = crs))

  filter_both(wskey, wsgcode, layer = layer, crs = crs)
}

#' Read features from FWA database layers.
#'
#' @inheritParams fwa_read_layer
#' @param layers A vector of valid character string layer names. See fwa_lookup_layer.
#' @param remove_empty A flag indicating whether to remove list elements of layers with no data.
#' @return A named list of sf objects.
#' If no data available and remove_empty = FALSE, value of list element will be NULL.
#' @examples
#'\dontrun{
#' fwa_read_layers(c("VICT"), layers = c("stream-network", "watersheds"), remove_empty = TRUE)
#' }
#' @export
fwa_read_layers <- function(x = NULL, ask = TRUE, layers = fwa_lookup_layer, crs = 3005, remove_empty = FALSE) {

  check_layers(layers)

  y <- list()
  for(i in layers){
    z <- try(fwa_read_layer(x = x, layer = i, ask = ask, crs = crs), silent = TRUE)
    if(inherits(y, "try-error")){
      y[[i]] <- z

    }
  }

  layers = c("a", "b")

  if(is.null(x)){
    if(!ask){
      return(all_data(layer))
    }
    if(yesno::yesno("This is a very large dataset. Do you want to download the entire ",
                    layer, " layer?")){
      return(all_data(layer))
    }
    return()
  }

  if(layer %in% lookup_layer$layer[!lookup_layer$WATERSHED_KEY]){
    check_wsgcode(x)
    return(filter_wsgcode(x, layer = layer, crs = crs))
  }

  check_wskey_wsgcode(x, layer = layer)
  wskey <- x[is_wskey(x)]
  wsgcode <- x[is_wsgcode(x)]

  if(length(wskey) & !length(wsgcode))
    return(filter_wskey(x, layer = layer, crs = crs))

  if(length(wsgcode) & !length(wskey))
    return(filter_wsgcode(x, layer = layer, crs = crs))

  filter_both(wskey, wsgcode, layer = layer, crs = crs)
}

#' Read from stream-network layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_LINESTRING geometry.
#' @examples
#' \dontrun{
#' fwa_read_stream_network(ask = FALSE)
#' fwa_read_stream_network(c(360709847, 360843586))
#' }
#' @export
fwa_read_stream_network <- function(x = NULL, ask = TRUE, crs = 3005) {
 fwa_read(x = x, ask = ask, layer = "stream-network", crs = crs)
}

#' Read from watersheds layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_watersheds(c(360709847, 360843586))
#' }
#' @export
fwa_read_watersheds <- function(x = NULL, ask = TRUE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "watersheds", crs = crs)
}

#' Read from coastlines layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_LINESTRING geometry.
#' @examples
#' \dontrun{
#' fwa_read_coastlines("GRAI")
#' }
#' @export
fwa_read_coastlines <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "coastlines", crs = crs)
}

#' Read from obstructions layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POINT geometry.
#' @examples
#' \dontrun{
#' fwa_read_obstructions("GRAI")
#' }
#' @export
fwa_read_obstructions <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "obstructions", crs = crs)
}

#' Read from linear-boundaries layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_LINESTRING geometry.
#' @examples
#' \dontrun{
#' fwa_read_linear_boundaries("GRAI")
#' }
#' @export
fwa_read_linear_boundaries <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "linear-boundaries", crs = crs)
}

#' Read from lakes layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_lakes("GRAI")
#' }
#' @export
fwa_read_lakes <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "lakes", crs = crs)
}

#' Read from rivers layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_rivers("GRAI")
#' }
#' @export
fwa_read_rivers <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "rivers", crs = crs)
}

#' Read from wetlands layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_wetlands("GRAI")
#' }
#' @export
#'
fwa_read_wetlands <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "wetlands", crs = crs)
}

#' Read from manmade-waterbodies layer.
#'
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' \dontrun{
#' fwa_read_manmade_waterbodies("GRAI")
#' }
#' @export
fwa_read_manmade_waterbodies <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "manmade-waterbodies", crs = crs)
}

#' Read from glaciers layer.
#'
#' @param x A vector of valid WATERSHED_GROUP_CODE. If NULL, entire dataset is read.
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' \dontrun{
#' fwa_read_glaciers("GRAI")
#' }
#' @export
fwa_read_glaciers <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "glaciers", crs = crs)
}

#' Read from watershed-groups layer.
#'
#' @param x A vector of valid WATERSHED_GROUP_CODE. If NULL, entire dataset is read.
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' \dontrun{
#' fwa_read_watershed_groups("GRAI")
#' }
#' @export
fwa_read_watershed_groups <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "watershed-groups", crs = crs)
}
