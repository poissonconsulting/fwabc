#' Search valid gnis names.
#'
#' @param pattern A character string containing a regular expression (or character string for fixed = TRUE) to be matched.
#' @param ignore_case A flag indicating whether to ignore case.
#' @param ... Arguments passed to grepl.
#' @return A character vector.
#' @examples
#' fwa_search_gnis("kaslo")
#' @export
fwa_search_gnis <- function(pattern, ignore_case = TRUE, ...){
  fwa_lookup_stream_gnis$GnisName[grepl(pattern, fwa_lookup_stream_gnis$GnisName, ignore.case = ignore_case, ...)]
}

#' Search valid watershed groups.
#'
#' @param pattern A character string containing a regular expression (or character string for fixed = TRUE) to be matched.
#' @param ignore_case A flag indicating whether to ignore case.
#' @param coast_only A flag indicating whether to only search watershed groups in the coastline layer.
#' @param code A flag indicating whether to search by watershed group code (not name).
#' @param ... Arguments passed to grepl.
#' @return A character vector.
#' @examples
#' fwa_search_wsgroup("graham")
#' @export
fwa_search_wsgroup <- function(pattern, ignore_case = TRUE, coast_only = FALSE, code = FALSE, ...){
  x <- fwa_lookup_wsgroup
  if(coast_only){
    x <- fwa_lookup_coast_wsgroup
  }
  y <- x$WatershedGroupName
  if(code){
    y <- x$WatershedGroupCode
  }
  y[grepl(pattern, y, ignore.case = ignore_case, ...)]
}

#' Generate and lable Rkm points at specified distance along FWA stream.
#'
#' @param stream A vector of valid GnisName, BlueLineKey or WatershedCode (see fwa_lookup_stream_gnis and fwa_lookup_stream_blkey reference).
#' @param distance A number indicating distance in metres between each point.
#' @param label_name A character string of the name of the new column containing km labels.
#' @param blkey_name A character string of the name of the new column containing BlueLineKey.
#' @param sfc_name A character string of the name of the new sfc column containing geometries.

#' @param dsn A character string indicating path to FWA_BC geodatabase.
#' @return A sf object.
#' @examples
#' kaslo_rkm <- fwa_rkm(stream = "Kaslo River", distance = 10)
#' @export
fwa_rkm <- function(stream = "Chown Brook", tributaries = TRUE, distance = 10, label_name = "Rkm", blkey_name = "BlueLineKey", sfc_name = "geometry"){
  dat <- fwa_stream(stream, tributaries = tributaries)

  do.call("rbind", lapply(unique(dat$BLUE_LINE_KEY), function(x){
    y <- line_rkm(dat[dat$BLUE_LINE_KEY == x,], distance = distance, label_name = label_name, sfc_name = sfc_name)
    y[[blkey_name]] <- x
    y
  }))
}




