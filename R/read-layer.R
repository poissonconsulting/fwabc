#' Read from stream-network layer.
#'
#' @param min_stream_order An integer indicating minimum STREAM_ORDER to read.
#' @inheritParams read_gkcn
#' @return A sf object with sfc_LINESTRING geometry.
#' @examples
#' \dontrun{
#' fwa_read_stream_network("GRAI")
#' }
#' @export
fwa_read_stream_network <- function(x = NULL, tributaries = FALSE,
                                    named_only = FALSE, min_stream_order = 1L,
                                    crs = 3005, collect = TRUE, check = TRUE) {

  check_integer(min_stream_order)

  x <- read_gkcn(layer = "stream-network", x = x,
                 named_only = named_only,
                 tributaries = tributaries,
                 crs = crs, collect = FALSE,
                 check = check)

  x <- x %>% bcdata::filter(STREAM_ORDER >= min_stream_order)

  if(collect)
    return(x %>% bcdata::collect())
  x
}

#' Read from coastlines layer.
#'
#' @inheritParams read_kcn
#' @return A sf object with sfc_LINESTRING geometry.
#' @examples
#' \dontrun{
#' fwa_read_coastlines("GRAI")
#' }
#' @export
fwa_read_coastlines <- function(x = NULL, crs = 3005, collect = TRUE, check = TRUE) {
  read_kcn(layer = "coastlines", x = x,
           tributaries = FALSE,
           crs = crs, collect = collect,
           check = check)
}

#' Read from watersheds layer.
#'
#' @inheritParams read_kcn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_watersheds("GRAI")
#' }
#' @export
fwa_read_watersheds <- function(x = NULL, tributaries = FALSE,
                                crs = 3005, collect = TRUE, check = TRUE) {
  read_kcn(layer = "watersheds", x = x,
           tributaries = tributaries,
           crs = crs, collect = collect,
           check = check)
}

#' Read from named-watersheds layer.
#'
#' @param min_stream_order An integer indicating minimum STREAM_ORDER to read.
#' @inheritParams read_gk
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_named_watersheds("GRAI")
#' }
#' @export
fwa_read_named_watersheds <- function(x = NULL, tributaries = FALSE,
                                      named_only = FALSE, min_stream_order = 1L,
                                      crs = 3005, collect = TRUE, check = TRUE) {
  check_integer(min_stream_order)

  x <- read_gk(layer = "named-watersheds", x = x,
                 named_only = named_only,
                 tributaries = tributaries,
                 crs = crs, collect = FALSE,
                 check = check)

  x <- x %>% bcdata::filter(STREAM_ORDER >= min_stream_order)

  if(collect)
    return(x %>% bcdata::collect())
  x
}

#' Read from manmade-waterbodies layer.
#'
#' @inheritParams read_gkcn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_manmade_waterbodies("GRAI")
#' }
#' @export
fwa_read_manmade_waterbodies <- function(x = NULL, tributaries = FALSE,
                                         named_only = FALSE, crs = 3005,
                                         collect = TRUE, check = TRUE) {
  read_gkcn(layer = "manmade-waterbodies",
            x = x,
            tributaries = tributaries,
            named_only - named_only,
            crs = crs,
            collect = collect,
            check = check)
}

#' Read from obstructions layer.
#'
#' @inheritParams read_gkcn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_obstructions("GRAI")
#' }
#' @export
fwa_read_obstructions <- function(x = NULL, tributaries = FALSE,
                                  named_only = FALSE, crs = 3005,
                                  collect = TRUE, check = TRUE) {
  read_gkcn(layer = "obstructions",
            x = x,
            tributaries = trinutaries,
            named_only = named_only,
            crs = crs,
            collect = collect,
            check = check)
}

#' Read from linear-boundaries layer.
#'
#' @inheritParams read_kcn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_linear_boundaries("GRAI")
#' }
#' @export
fwa_read_linear_boundaries <- function(x = NULL, crs = 3005,
                                  collect = TRUE, check = TRUE) {
  read_kcn(layer = "linear-boundaries",
            x = x,
            tributaries = FALSE,
            crs = crs,
            collect = collect,
            check = check)
}

#' Read from lakes layer.
#'
#' @inheritParams read_gkcn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_lakes("GRAI")
#' }
#' @export
fwa_read_lakes <- function(x = NULL, tributaries = FALSE,
                                  named_only = FALSE, crs = 3005,
                                  collect = TRUE, check = TRUE) {
  read_gkcn(layer = "lakes",
            x = x,
            tributaries = tributaries,
            named_only = named_only,
            crs = crs,
            collect = collect,
            check = check)
}

#' Read from rivers layer.
#'
#' @inheritParams read_gkcn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_rivers("GRAI")
#' }
#' @export
fwa_read_rivers <- function(x = NULL, tributaries = FALSE,
                           named_only = FALSE, crs = 3005,
                           collect = TRUE, check = TRUE) {
  read_gkcn(layer = "rivers",
            x = x,
            tributaries = tributaries,
            named_only = named_only,
            crs = crs,
            collect = collect,
            check = check)
}

#' Read from wetlands layer.
#'
#' @inheritParams read_gkcn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_wetlands("GRAI")
#' }
#' @export
fwa_read_wetlands <- function(x = NULL, tributaries = FALSE,
                           named_only = FALSE, crs = 3005,
                           collect = TRUE, check = TRUE) {
  read_gkcn(layer = "wetlands",
            x = x,
            tributaries = tributaries,
            named_only = named_only,
            crs = crs,
            collect = collect,
            check = check)
}

#' Read from glaciers layer.
#'
#' @inheritParams read_kcn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_glaciers("GRAI")
#' }
#' @export
fwa_read_glaciers <- function(x = NULL, crs = 3005,
                                       collect = TRUE, check = TRUE) {
  read_kcn(layer = "glaciers",
           x = x,
           tributaries = FALSE,
           crs = crs,
           collect = collect,
           check = check)
}

#' Read from watershed-groups layer.
#'
#' @inheritParams read_cn
#' @return A sf object with sfc_POLYGON geometry.
#' @examples
#' \dontrun{
#' fwa_read_watershed_groups("GRAI")
#' }
#' @export
fwa_read_watershed_groups <- function(x = NULL, crs = 3005,
                              collect = TRUE, check = TRUE) {
  read_cn(layer = "glaciers",
           x = x,
           tributaries = FALSE,
           crs = crs,
           collect = collect,
           check = check)
}




