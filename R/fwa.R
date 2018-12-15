#' Read stream polylines from FWA_ROUTES_SP layer.
#'
#' @param stream A vector of valid GnisName, BlueLineKey or WatershedCode (see fwa_lookup_stream_gnis and fwa_lookup_stream_blkey reference).
#' @param tributaries A flag indicating whether to include tributaries.
#' @param dsn A character string indicating path to FWA database with FWA_ROUTES_SP layer.
#' @return A linestring sf object.
#' @examples
#' streams <- fwa_stream(c("Sangan River", "Hiellen River"))
#' sangan_tribs <- fwa_stream("Sangan River", tributaries = TRUE)
#' @export
fwa_stream <- function(stream = "Sangan River", tributaries = FALSE, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb") {
  check_dsn(dsn, "FWA_ROUTES_SP")
  check_stream(stream)

  if(tributaries){
    x <- stream_to_wscode(stream) %>% tribs_stream()
  } else {
    x <- stream_to_blk(stream)
  }

  sql <- paste("select * from FWA_ROUTES_SP where",
               gsub('.{0,4}$', '', paste0("BLUE_LINE_KEY = '", x, "' OR ", collapse = "")))
  st_read(dsn = dsn, layer = "FWA_ROUTES_SP", query = sql)
}

#' Read coastline polylines from FWA_COASTLINES_SP layer.
#'
#' Select particular coastlines with coastline argument.
#'
#' @param coastline A vector of valid coastline WatershedGroupCode, WatershedGroupName, BlueLineKey, and/or WatershedCode. (see fwa_coastline_wsg_lookup and fwa_coastline_blk_lookup for reference).
#' @param dsn A character string indicating path to FWA database with FWA_COASTLINE_SP layer.
#' @return A linestring sf object.
#' @examples
#' graham <- fwa_coastline("Graham Island")
#' @export
fwa_coastline <- function(coastline = "Graham Island", dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb") {
  check_dsn(dsn, layer = "FWA_COASTLINES_SP")
  check_coastline(coastline)

  cl <- wsgname_to_wsgcode(coastline)

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
  })

  sql <- paste("select * from FWA_COASTLINES_SP where", gsub('.{0,4}$', '', paste(sql, collapse = "")))
  st_read(dsn = dsn, layer = "FWA_COASTLINES_SP", query = sql)
}

#' Read watershed group polygons from FWA_WATERSHED_GROUPS_POLY layer.
#'
#' Select particular streams with stream argument. Add all tributaries with tributaries = TRUE
#'
#' @param watershed_group A vector of valid WatershedGroupCode and/or WatershedGroupNames (see fwa_wsgroup_lookup reference).
#' @param dsn A character string indicating path to FWA database with FWA_ROUTES_SP layer.
#' @return A polygon sf object.
#' @examples
#' grai <- fwa_watershed_group("Graham Island")
#' @export
fwa_watershed_group <- function(watershed_group = "Graham Island", dsn) {
  check_dsn(dsn, "FWA_WATERSHED_GROUPS_POLY")
  check_wsgroup(watershed_group)

  ws <- wsgname_to_wsgcode(watershed_group)

  sql <- paste("select * from FWA_WATERSHED_GROUPS_POLY where",
               gsub('.{0,4}$', '', paste0("WATERSHED_GROUP_CODE = '", ws, "' OR ", collapse = "")))
  st_read(dsn = dsn, layer = "FWA_WATERSHED_GROUPS_POLY", query = sql)
}

#' Read watershed polygons from FWA_WATERSHEDS_POLY database layers.
#'
#' @param watershed A vector of valid GnisName, or WatershedCode within a single WatershedGroup (see fwa_lookup_stream_gnis for reference).
#' @param watershed_group A character string of the WatershedGroupName or WatershedGroupCode containing watershed(s).
#' @param tributaries A flag indicating whether to include tributaries.
#' @param dsn A character string indicating path to FWA_WATERSHEDS_POLY geodatabase.
#' @return A polygon sf object.
#' @examples
#' wsheds <- fwa_watershed(c("Hiellen River", "Sangan River"), "Graham Island", tributaries = TRUE)
#' @export
fwa_watershed <- function(watershed = "Sangan River", watershed_group = "Graham Island", tributaries = FALSE, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb") {

  if(length(watershed_group) > 1) err("Only one watershed group allowed. All watersheds should be within that group.")
  check_watershed(watershed)
  check_wsgroup(watershed_group)
  group <- wsgname_to_wsgcode(watershed_group)
  check_dsn(dsn, group)

  codes <- watershed_to_wscode(watershed, group)
  if(tributaries){
    codes <- codes %>% tribs_wshed()
  }

  sql <- paste("select * from", group, "where",
               gsub('.{0,4}$', '', paste0("FWA_WATERSHED_CODE = '", codes, "' OR ", collapse = "")))
  st_read(dsn = dsn, layer = group, query = sql)
}
