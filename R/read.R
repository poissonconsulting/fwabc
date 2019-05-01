#' Read features from FWA database layer.
#'
#' @param x A vector of valid WATERSHED_KEY or WATERSHED_GROUP_CODE. If NULL, entire dataset is read.
#' @param layer A character string of valid layer name. See fwa_lookup_layers.
#' @param ask A flag indicating whether to ask before reading entire dataset.
#' @param crs The epsg code for the coordinate reference system. Defaults to `3005`
#'        (B.C. Albers). See https://epsgi.io.
#' @return A sf object.
#' @examples
#' fwa_read(c(360709847, 360843586), layer = "stream-network")
#' @export
fwa_read <- function(x = NULL, ask = TRUE, layer = "stream-network", crs = 3005) {

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

  check_wskey_wsgcode(x, layer = layer)
  x <- wsgcode_to_wskey(x, layer = layer)

  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", layer), crs = crs) %>%
    bcdata::filter(WATERSHED_KEY %in% x) %>%
    bcdata::collect()
}

#' Read from stream-network layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_LINESTRING geometry.
#' @examples
#' fwa_read_streams()
#' fwa_read_streams(c(360709847, 360843586))
#' @export
fwa_read_streams <- function(x = NULL, ask = TRUE, crs = 3005) {
 fwa_read(x = x, ask = ask, layer = "stream-network", crs = crs)
}

#' Read from watersheds layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' fwa_read_watersheds(c(360709847, 360843586))
#' @export
fwa_read_watersheds <- function(x = NULL, ask = TRUE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "watersheds", crs = crs)
}

#' Read from coastlines layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_LINESTRING geometry.
#' @examples
#' fwa_read_coastlines("GRAI")
#' @export
fwa_read_coastlines <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "coastlines", crs = crs)
}

#' Read from obstructions layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POINT geometry.
#' @examples
#' fwa_read_obstructions("GRAI")
#' @export
fwa_read_obstructions <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "obstructions", crs = crs)
}

#' Read from linear-boundaries layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_LINESTRING geometry.
#' @examples
#' fwa_read_linear_boundaries("GRAI")
#' @export
fwa_read_linear_boundaries <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "linear-boundaries", crs = crs)
}

#' Read from lakes layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' fwa_read_lakes("GRAI")
#' @export
fwa_read_lakes <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "lakes", crs = crs)
}

#' Read from rivers layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' fwa_read_rivers("GRAI")
#' @export
fwa_read_rivers <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "rivers", crs = crs)
}

#' Read from wetlands layer.
#'
#' @inheritParams fwa_read
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' fwa_read_wetlands("GRAI")
#' @export
fwa_read_wetlands <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "wetlands", crs = crs)
}

#' Read from manmade-waterbodies layer.
#'
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' fwa_read_manmade_waterbodies("GRAI")
#' @export
fwa_read_manmade_waterbodies <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "manmade-waterbodies", crs = crs)
}

#' Read from glaciers layer.
#'
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' fwa_read_glaciers("GRAI")
#' @export
fwa_read_glaciers <- function(x = NULL, ask = FALSE, crs = 3005) {
  fwa_read(x = x, ask = ask, layer = "glaciers", crs = crs)
}

#' Read from watershed-groups layer.
#'
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' fwa_read_watershed_group("GRAI")
#' @export
fwa_read_watershed_groups <- function(x = NULL, ask = FALSE, crs = 3005) {

  layer <- "watershed-groups"

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

  check_wsgcode(x, layer = layer)
  x <- wskey_to_wsgcode(x, layer = layer)

  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", layer), crs = crs) %>%
    bcdata::filter(WATERSHED_GROUP_CODE %in% x) %>%
    bcdata::collect()
}
