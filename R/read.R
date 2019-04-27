#' Read features from FWA database layer.
#'
#' @param x A vector of valid WATERSHED_KEY or WATERSHED_GROUP_CODE. If NULL, entire dataset is read.
#' @param layer A character string of valid layer name (see fwa_lookup_layers for reference).
#' @param ask A flag indicating whether to ask before reading entire dataset.
#' @return A sf object.
#' @examples
#' streams <- fwa_read_layer(c(360709847, 360843586), layer = "stream-network")
#' @export
fwa_read <- function(x = NULL, ask = TRUE, layer = "stream-network") {

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

  check_wskey_wsgcode(x)
  x <- wsgcode_to_wskey(x)

  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", layer)) %>%
    bcdata::filter(WATERSHED_KEY %in% c(360709847, 360843586)) %>%
    bcdata::collect()
}

#' Read from stream-network layer.
#'
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' all <- fwa_read_stream()
#' streams <- fwa_read_stream(c(360709847, 360843586))
#' @export
fwa_read_stream <- function(x = NULL, ask = TRUE) {
 fwa_read(x = x, ask = ask, layer = "stream-network")
}

#' Read from watersheds layer.
#'
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' watershed <- fwa_read_watershed(c(360709847, 360843586))
#' @export
fwa_read_watersheds <- function(x = NULL, ask = TRUE) {
  fwa_read(x = x, ask = ask, layer = "watersheds")
}

#' Read from coastlines layer.
#'
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' coastline <- fwa_read_coastline("GRAI")
#' @export
fwa_read_coastlines <- function(x = NULL, ask = FALSE) {
  fwa_read(x = x, ask = ask, layer = "coastlines")
}

#' Read from watershed-groups layer.
#'
#' @param x A vector of valid WATERSHED_GROUP_CODE. If NULL, entire dataset is read.
#' @inheritParams fwa_read
#' @return A sf object.
#' @examples
#' graham <- fwa_read_watershed_group("GRAI")
#' @export
fwa_read_watershed_groups <- function(x = NULL, ask = FALSE) {
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

  check_wsgcode(x)

  bcdata::bcdc_query_geodata(paste0("freshwater-atlas-watershed-groups")) %>%
    bcdata::filter(WATERSHED_KEY %in% "PORI") %>%
    bcdata::collect()
}
