#' Search valid GnisNames.
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
#' @param coast A flag indicating whether to only search watershed groups in the coastline layer.
#' @param code A flag indicating whether to search by watershed group code (not name).
#' @param ... Arguments passed to grepl.
#' @return A character vector.
#' @examples
#' fwa_search_wsgroup("graham")
#' @export
fwa_search_wsgroup <- function(pattern, ignore_case = TRUE, coast = FALSE, code = FALSE, ...){
  x <- fwa_lookup_wsgroup
  if(coast){
    x <- fwa_lookup_coast_wsgroup
  }
  y <- x$WatershedGroupName
  if(code){
    y <- x$WatershedGroupCode
  }
  y[grepl(pattern, y, ignore.case = ignore_case, ...)]
}
