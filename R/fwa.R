#' Read stream data from FWA_ROUTES_SP layer.
#'
#' Select particular streams with stream argument. Add all tributaries with tributaries = TRUE
#'
#'
#' @param stream A vector of valid BlueLineKey/GnisName/WatershedCode in any combination (see fwa_stream_lookup reference).
#' @param tributaries A flag indicating whether to include all (TRUE) or no (FALSE) tributaries.
#' @param dsn A character string indicating path to FWA database with FWA_ROUTES_SP layer.
#' @return A sf object.
#' @examples
#' kaslo_tribs <- fwa_stream("Kaslo River", tributaries = TRUE)
#' @export
fwa_stream <- function(stream = "Kaslo River", tributaries = FALSE, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb") {
  check_dsn(dsn, layer = "FWA_ROUTES_SP")
  check_stream(stream)

  if(tributaries){
    x <- stream_to_wscode(stream) %>% tribs()
  } else {
    x <- stream_to_blk(stream)
  }

  or <- paste0("BLUE_LINE_KEY = '", x, "' OR ", collapse = "") %>% gsub('.{0,4}$', '', .)
  sql <- paste("select * from FWA_ROUTES_SP where", or)

  st_read(dsn = dsn, layer = "FWA_ROUTES_SP", query = sql)
}

#' Read coastline data from FWA_COASTLINES_SP layer.
#'
#' Select particular coastlines with coastline argument.
#'
#' @param coastline A vector of valid coastline WatershedGroupCode, WatershedGroupName, BlueLineKey, and/or WatershedCode. (see fwa_coastline_wsg_lookup and fwa_coastline_blk_lookup for reference).
#' @param dsn A character string indicating path to FWA database with FWA_COASTLINE_SP layer.
#' @return A sf object.
#' @examples
#' porcher <- fwa_coastline("Porcher Island")
#' @export
fwa_coastline <- function(coastline, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb") {
  check_dsn(dsn, layer = "FWA_COASTLINES_SP")
  check_coastline(coastline)

  cl <- unlist(lapply(coastline, function(x){
    if(is_wsg_name_coast(x)){
      return(wsgname_to_wsgcode(x))
    }
    x
  }))

  sql <- lapply(cl, function(x){
    if(is_wsg_code_coast(x)){
      return(paste0("WATERSHED_GROUP_CODE = '", x, "' OR "))
    }
    if(is_blk_coast(x)){
      return(paste0("BLUE_LINE_KEY = ", x, " OR "))
    }
    if(is_ws_code_coast(x)){
      return(paste0("FWA_WATERSHED_CODE = '", x, "' OR "))
    }
  }) %>%
    paste(collapse = "") %>%
    gsub('.{0,4}$', '', .) %>%
    paste("select * from FWA_COASTLINES_SP where", .)

  st_read(dsn = dsn, layer = "FWA_COASTLINES_SP", query = sql)
}
