#' Read stream polylines from FWA_ROUTES_SP layer.
#'
#' @param x A vector of valid GNIS_NAME, BLUE_LINE_KEY or FWA_WATERSHED_CODE from FWA_ROUTES_SP layer in FWA_BC geodatabase (see fwa_lookup_stream for reference).
#' @param tributaries A flag indicating whether to include tributaries or an integer indicating order of tributaries to include. If TRUE, all tributaries are returned.
#' @param dsn A character string indicating path to FWA_BC geodatabase.
#' @param ask A flag indicating whether to ask before reading entire dataset.
#' @return A linestring sf object.
#' @examples
#' all <- fwa_streams()
#' streams <- fwa_stream(c("Sangan River", "Hiellen River"))
#' sangan_tribs <- fwa_stream("Sangan River", tributaries = 1)
#' @export
fwa_stream <- function(x = NULL, tributaries = FALSE, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb", ask = TRUE) {
  check_dsn(dsn, "FWA_ROUTES_SP")
  check_tributaries(tributaries)

  if(is.null(x)){
    if(!ask){
      return(st_read(dsn = dsn, layer = "FWA_ROUTES_SP"))
    }
    if(yesno::yesno("This is a very large dataset. Are you sure you want all FWA streams?")){
      st_read(dsn = dsn, layer = "FWA_ROUTES_SP")
    }
  }
  check_stream(x)

  if(tributaries){
    if(is.integer(tributaries)) {
      x <- stream_to_wscode(x) %>% tribs_streams(n = tributaries)
    } else {
      # get all trib orders (never more than 100)
      x <- stream_to_wscode(x) %>% tribs_streams(n = 100)
    }
    } else {
    x <- stream_to_blk(x)
  }

  sql <- paste("select * from FWA_ROUTES_SP where",
               gsub('.{0,4}$', '', paste0("BLUE_LINE_KEY = '", x, "' OR ", collapse = "")))
  st_read(dsn = dsn, layer = "FWA_ROUTES_SP", query = sql)
}

#' Read coastline polylines from FWA_COASTLINES_SP layer.
#'
#' Select particular coastlines with coastline argument.
#'
#' @param x A vector of valid coastline WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME, BLUE_LINE_KEY, or FWA_WATERSHED_CODE from FWA_COASTLINES_SP layer of FWA_BC geodatabase (see fwa_lookup_coastline for reference).
#' @inheritParams fwa_stream
#' @return A linestring sf object.
#' @examples
#' all <- fwa_coastline()
#' graham <- fwa_coastline("Graham Island")
#' @export
fwa_coastline <- function(x = NULL, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb") {
  check_dsn(dsn, layer = "FWA_COASTLINES_SP")

  if(is.null(x)){
    return(st_read(dsn = dsn, layer = "FWA_COASTLINES_SP"))
  }

  check_coastline(x)
  cl <- wsgname_to_wsgcode(x)

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
#' @param x A vector of valid WATERSHED_GROUP_CODE or WATERSHED_GROUP_NAME (see fwa_lookup_watershedgroup reference).
#' @inheritParams fwa_stream
#' @return A polygon sf object.
#' @examples
#' all <- fwa_watershed_group()
#' grai <- fwa_watershed_group("Graham Island")
#' @export
fwa_watershed_group <- function(x = NULL, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb/") {
  check_dsn(dsn, "FWA_WATERSHED_GROUPS_POLY")

  if(is.null(x)){
    return(st_read(dsn = dsn, layer = "FWA_WATERSHED_GROUPS_POLY"))
  }
  check_wsgroup(x)
  ws <- wsgname_to_wsgcode(x)

  sql <- paste("select * from FWA_WATERSHED_GROUPS_POLY where",
               gsub('.{0,4}$', '', paste0("WATERSHED_GROUP_CODE = '", ws, "' OR ", collapse = "")))
  st_read(dsn = dsn, layer = "FWA_WATERSHED_GROUPS_POLY", query = sql)
}

#' Read watershed polygons from FWA_WATERSHEDS_POLY database layers.
#'
#' @param x A vector of valid GNIS_NAME, FWA_WATERSHED_CODE, WATERSHED_GROUP_CODE or WATERSHED_GROUP_NAME from FWA_WATERSHEDS_POLY geodatabase
#' @param tributaries A flag indicating whether to include tributaries or an integer indicating order of tributaries to include. If TRUE, all tributaries are returned.
#' If x is a WATERSHED_GROUP_CODE or WATERSHED_GROUP_NAME, tributaries argument is ignored.
#' @param dsn A character string indicating path to FWA_WATERSHEDS_POLY geodatabase.
#' @return A polygon sf object.
#' @examples
#' graham_wsgroup <- fwa_watershed("Graham Island")
#' watersheds <- fwa_watershed(c("Hiellen River", "Sangan River"), tributaries = 1L)
#' @export
fwa_watershed <- function(x = NULL, tributaries = FALSE, dsn = "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb") {

  check_dsn(dsn)
  check_tributaries(tributaries)

  if(is.null(x)){
    return(st_read(dsn = dsn, layer = "PORI"))
  }

  check_watershed(x)
  if(is_wsg_code(x) | is_wsg_name(x)){
    x <- wsgname_to_wsgcode(x)

  }
  wscodes <- watershed_to_wscode(x, group)


  if(tributaries){
    if(is.integer(tributaries)) {
      wscodes <- wscodes %>% tribs_wshed(n = tributaries)
    } else {
      # get all trib orders (never more than 100)
      wscodes <- wscodes %>% tribs_wshed(n = 100)
    }
  }

  sql <- paste("select * from", group, "where",
               gsub('.{0,4}$', '', paste0("FWA_WATERSHED_CODE = '", wscodes, "' OR ", collapse = "")))
  st_read(dsn = dsn, layer = group, query = sql)
}
