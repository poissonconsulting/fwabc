#' Select and read FWA_ROUTES_SP by stream.
#'
#' @param stream A vector of valid BlueLineKey/GnisName/WatershedCode in any combination (see fwa_stream_lookup reference).
#' @param tributaries A flag indicating whether to include all (TRUE) or no (FALSE) tributaries.
#' @param dsn A character string indicating path to FWA database with FWA_ROUTES_SP layer.
#' @param
#' @return A sf object.
#' @examples
#' kaslo_tribs <- fwa_stream(stream = "Kaslo River", tributaries = TRUE)
#' @export
fwa_stream <- function(stream = "Kaslo River", tributaries = FALSE, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb") {
  check_dsn(dsn, layer = "FWA_ROUTES_SP")
  check_stream(stream)

  if(tributaries){
    x <- stream_to_wscode(stream) %>%
      fwa_tributaries()
  } else {
    x <- stream_to_blk(stream)
  }

  or <- paste0("BLUE_LINE_KEY = '", x, "' OR ", collapse = "") %>% gsub('.{0,4}$', '', .)
  sql <- paste("select * from FWA_ROUTES_SP where", or)

  st_read(dsn = dsn, layer = "FWA_ROUTES_SP", query = sql)
}

#'
#'
#' #' Select and read FWA_COASTLINES_SP by BlueLineKey and/or GnisName
#' #'
#' #' @param dsn A character string indicating path to FWA database with FWA_ROUTES_SP layer.
#' #' @param stream A vector of strings of valid BlueLineKey and/or GnisName (see fwa_blue_line_key and fwa_gnis_name for reference).
#' #' @param watershed_group A vector of strings of valid WatershedGroupCode or WatershedGroupName (see fwa_watershed_group for reference).
#' #' @param
#' #' @return A sf object.
#' #' @examples
#' #' kaslo <- ps_fwa_coastline(gnis_name = "Kaslo River")
#' #' stream <- ps_fwa_coastline(blue_line_key = fwa_blue_line_key$BlueLineKey[1:10,], gnis_name = fwa_gnis$GnisName[1:10,])
#' #' @export
#' fwa_coastline <- function(dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb", blue_line_key = NULL, watershed_group = NULL) {
#'   check_dsn(dsn, layer = "FWA_COASTLINES_SP")
#'   check_blk_coast(blue_line_key)
#'   check_ws(watershed_group)
#'
#'   blk <- blue_line_key
#'   blk_or <- character(0)
#'   if(!is.null(blk)){
#'     blk_or <- or_sql(blk)
#'   }
#'
#'   ws <- wsname_to_wscode(watershed_group)
#'   ws_or <- character(0)
#'   if(!is.null(ws)){
#'     ws_or <- or_sql(ws, "WATERSHED_GROUP_CODE")
#'   }
#'
#'   sql <- paste0(blk_or, ws_or) %>% snip %>% select_sql(layer = "FWA_COASTLINES_SP")
#'   st_read(dsn = dsn, layer = "FWA_COASTLINES_SP", query = sql)
#' }
#'
#' #' Select and read FWA_WATERSHED_GROUPS_POLY by WatershedGroupCode and/or WatershedGroupName.
#' #'
#' #' @param dsn A character string indicating path to FWA database with FWA_ROUTES_SP layer.
#' #' @param watershed_group A character string indicating WatershedGroupName or WatershedGroupCode.
#' #' @param
#' #' @return A sf object.
#' #' @examples
#' #' kootenay <- ps_fwa_watershed_group(watershed_group = "Kootenay Lake")
#' #' @export
#' fwa_watershed_group <- function(dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb", watershed_group = "Kootenay Lake") {
#'   check_dsn(dsn, layer = "FWA_WATERSHED_GROUPS_POLY")
#'   check_ws(watershed_group)
#'
#'   ws <- wsname_to_wscode(watershed_group)
#'   or <- or_sql(ws, "WATERSHED_GROUP_CODE") %>% snip
#'
#'   st_read(dsn = dsn, layer = "FWA_WATERSHED_GROUPS_POLY",
#'           query = select_sql(or, layer = "FWA_WATERSHED_GROUPS_POLY"))
#' }
#'
#' #' Select and read a layer from FWA_WATERSHED_BOUNDARIES_SP geodatabase by WatershedGroupCode or WatershedGroupName.
#' #'
#' #' @param dsn A character string indicating path to FWA_WATERSHED_BOUNDARIES_SP geodatabase.
#' #' @param watershed_group A character string indicating WatershedGroupName or WatershedGroupCode.
#' #' @param
#' #' @return A sf object.
#' #' @examples
#' #' kootenay <- ps_fwa_watershed_line(watershed_group = "Kootenay Lake")
#' #' @export
#' fwa_watershed_line <- function(dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHED_BOUNDARIES_SP.gdb/", watershed_group = "Kootenay Lake") {
#'   check_string(watershed_group)
#'   check_ws(watershed_group)
#'   ws <- wsname_to_wscode(watershed_group)
#'   check_dsn(dsn, layer = ws)
#'
#'   st_read(dsn = dsn, layer = ws)
#' }
#'
#' #' Select and read a layer from FWA_WATERSHEDS_POLY geodatabase by WatershedGroupCode or WatershedGroupName.
#' #'
#' #' @param dsn A character string indicating path to FWA_WATERSHEDS_POLY geodatabase.
#' #' @param watershed_group A character string indicating WatershedGroupName or WatershedGroupCode.
#' #' @param
#' #' @return A sf object.
#' #' @examples
#' #' kootenay <- ps_fwa_watershed_poly(watershed_group = "Kootenay Lake")
#' #' @export
#' fwa_watershed_poly <- function(dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb/", watershed_group = "Kootenay Lake") {
#'   check_string(watershed_group)
#'   check_ws(watershed_group)
#'   ws <- wsname_to_wscode(watershed_group)
#'   check_dsn(dsn, layer = ws)
#'
#'   st_read(dsn = dsn, layer = ws)
#' }
#'
#' #' Generate and lable Rkm points at specified distance along FWA stream.
#' #'
#' #' @param dsn A character string indicating path to FWA_BC geodatabase.
#' #' @param stream A vector of strings of valid BlueLineKey and/or GnisName (see fwa_blue_line_key and fwa_gnis_name for reference).
#' #' @param distance A number indicating distance in metres between each point.
#' #' @return A sf object.
#' #' @examples
#' #' kaslo_rkm <- ps_fwa_rkm(stream = "Kaslo River", distance = 10)
#' #' @export
#' ps_fwa_rkm <- function(dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb/", stream = "Kaslo River", distance = 10, label_name = "Rkm"){
#'                        start <- stream %>%
#'                          st_cast("POINT") %>%
#'                          .[which.min(ps_sfc_to_coords(.)$Z),]
#'
#'                        pts <- ps_line_sample(stream, distance)
#'                        n <- nrow(pts)
#'                        i <- st_nearest_feature(start %>% st_zm(), pts %>% st_zm())
#'                        label <- seq(0, n*distance, distance)/1000
#'                        if(i > n/2){
#'                          label <- rev(label)
#'                        }
#'                        pts[[label_name]] <- label[1:n]
#'                        pts
#'                        }
#'
#'
#'
