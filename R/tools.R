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
#' @param dsn A character string indicating path to FWA_BC geodatabase.
#' @return A sf object.
#' @examples
#' kaslo_rkm <- ps_fwa_rkm(stream = "Kaslo River", distance = 10)
#' @export
ps_fwa_rkm <- function(stream = "Kaslo River", distance = 10, label_name = "Rkm"){
  x <- fwa_stream(stream)
  rkm <- line_rkm(x)
}


