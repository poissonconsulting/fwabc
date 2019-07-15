#' Read features from 'freshwater-atlas-watersheds' dataset.
#'
#' @param x A vector of valid GNIS_NAME or WATERSHED_KEY.
#' If NULL, entire dataset is read.
#' @param tributaries A flag indicating whether to read all tributaries in addition
#'  (only applicable if x is GNIS_NAME or WATERSHED_KEY).
#' @param min_watershed_order An integer indicating minimum WATERSHED_ORDER to read.
#' @param crs The epsg code for the coordinate reference system. Defaults to `3005`
#'        (B.C. Albers). See https://epsgi.io.
#' @param collect A flag indicating whether to collect result.
#' @return A sf object if collect = TRUE, otherwise a object of class bcdc_promise.
#' @examples
#'\dontrun{
#' fwa_read_watersheds("Upper Fraser River")
#' }
#' @export
fwa_read_watersheds <- function(x = NULL,
                                tributaries = FALSE,
                                min_watershed_order = 1L,
                                crs = 3005,
                                collect = TRUE) {

  if(!is.null(x)){
    check_x(x, "gk")
    check_x_layer(x, "gk", "named-watersheds")
  }

  check_flag(tributaries)
  check_integer(min_watershed_order)
  check_numeric(crs)

  if(is_wsgname(x[1])){
    x <- wsgname_to_wsgcode(x)
  }

  what <- what_is_it(x[1])
  x <- read_layer(what = what,
                  layer = "named-watersheds",
                  x = x,
                  named_only = FALSE,
                  tributaries = tributaries,
                  multiple_gnis = TRUE,
                  crs = crs) %>%
    bcdata::filter(WATERSHED_ORDER >= min_watershed_order)

  if(collect)
    return(x %>% bcdata::collect())
  x
}



