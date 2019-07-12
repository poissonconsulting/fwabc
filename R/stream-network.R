#' Read features from 'freshwater-atlas-stream-network' dataset.
#'
#' @param x A vector of valid GNIS_NAME, WATERSHED_KEY, WATERSHED_GROUP_NAME or WATERSHED_GROUP_CODE.
#' If NULL, entire dataset is read.
#' @param named_only A flag indicating whether to only include features with GNIS_NAME.
#' @param tributaries A flag indicating whether to read all tributaries in addition
#'  (only applicable if x is GNIS_NAME or WATERSHED_KEY).
#' @param min_stream_order An integer indicating minimum STREAM_ORDER to read.
#' @param crs The epsg code for the coordinate reference system. Defaults to `3005`
#'        (B.C. Albers). See https://epsgi.io.
#' @param collect A flag indicating whether to collect result.
#' @return A sf object if collect = TRUE, otherwise a object of class bcdc_promise.
#' @examples
#'\dontrun{
#' fwa_read_stream_network("Fraser River")
#' }
#' @export
fwa_read_stream_network <- function(x = NULL,
                                    named_only = FALSE,
                                    tributaries = FALSE,
                                    min_stream_order = 1L,
                                    crs = 3005,
                                    collect = TRUE) {

  if(!is.null(x)){
    check_gkcn(x)
    check_gkcn_layer(x, "stream-network")
  }

  check_flag(named_only)
  check_flag(tributaries)
  check_integer(min_stream_order)
  check_numeric(crs)

  if(is_wsgname(x[1])){
    x <- wsgname_to_wsgcode(x)
  }

  what <- what_is_it(x)[1]
  x <- read_layer(what, "stream-network", x, named_only,
                  tributaries, min_stream_order, crs)

  if(collect)
    return(x %>% bcdata::collect())
  x
}



