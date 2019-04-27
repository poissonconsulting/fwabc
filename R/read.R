#' Read layer from FWA database.
#'
#' @param x A vector of valid WATERSHED_KEY or WATERSHED_GROUP_CODE.
#' @param layer A character string of valid layer name (see fwa_lookup_layers for reference).
#' @param ask A flag indicating whether to ask before reading entire dataset.
#' @return A sf object.
#' @examples
#' streams <- fwa_read_layer(c(360709847, 360843586), layer = "stream-network")
#' @export
fwa_read <- function(x = NULL, layer = "stream-network", ask = TRUE) {

  check_layer(layer)

  if(is.null(x)){
    if(!ask){
      return(all_data(layer))
    }
    if(yesno::yesno("This is a very large dataset. Do you want to download the entire ",
                    layer, " layer?")){
      all_data(layer)
    }
  }
  check_wskey_wsgcode(x)
  x <- wsgcode_to_wskey(x)

  some_data(layer, x)
}

#' Read features from stream-network layer.
#'
#' @param x A vector of valid WATERSHED_KEY or WATERSHED_GROUP_CODE.
#' @param ask A flag indicating whether to ask before reading entire dataset.
#' @return A sf object.
#' @examples
#' all <- fwa_read_stream()
#' streams <- fwa_read_stream(c(360709847, 360843586))
#' @export
fwa_read_stream <- function(x = NULL, ask = TRUE) {
  fwa_read(x = x, ask = ask, layer = "stream-network")
}

#' Read watershed from FWA_WATERSHEDS_POLY geodatabase.
#'
#' @inheritParams fwa_read_stream
#' @return A sf object.
#' @examples
#' watershed <- fwa_read_watershed(c(360709847, 360843586))
#' @export
fwa_read_watershed <- function(x = NULL, ask = TRUE) {
  fwa_read(x = x, ask = ask, layer = "watersheds")
}

#' Read coastline from FWA_COASTLINES_SP layer in FWA_BC geodatabase.
#'
#' @inheritParams fwa_read_stream
#' @return A sf object.
#' @examples
#' coastline <- fwa_read_coastline("GRAI")
#' @export
fwa_read_coastline <- function(x = NULL, ask = FALSE) {
  fwa_read(x = x, ask = ask, layer = "coastline")
}

#' Read watershed group from FWA_WATERSHED_GROUPS layer in FWA_BC geodatabase
#'
#' @inheritParams fwa_read_stream
#' @return A sf object.
#' @examples
#' graham <- fwa_read_watershed_group("GRAI")
#' @export
fwa_read_watershed_group <- function(x = NULL, ask = FALSE) {
  fwa_read(x = x, ask = ask, layer = "watershed-groups")
}
