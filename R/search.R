#' Search valid GNIS_NAME.
#'
#' @param pattern A character string containing a regular expression (or character string for fixed = TRUE) to be matched.
#' @param layer A valid layer from fwa_lookup_layer.
#' @param ... Arguments passed to grepl.
#' @return A character vector.
#' @examples
#' fwa_search_gnis_name("sangan")
#' @export
fwa_search_gnis <- function(pattern, layer = NULL, ...){
  check_layer(layer)

  gnis <- lookup_gnis$GNIS_NAME
  if(!is.null(layer)){
    gnis <- lookup_gnis$GNIS_NAME[lookup_gnis[[layer]]]
  }
  gnis[grepl(pattern, gnis, ...)]
}

#' Search valid WATERSHED_GROUP_NAME.
#'
#' @inheritParams fwa_search_gnis
#' @return A character vector.
#' @examples
#' fwa_search_watershed_group("graham")
#' @export
fwa_search_watershed_group <- function(pattern, layer = NULL, ...){
  wsgroup <- lookup_wsgroup$WATERSHED_GROUP_NAME
  if(!is.null(layer)){
    wsgroup <- lookup_wsgroup$WATERSHED_GROUP_NAME[lookup_wsgroup[[layer]]]
  }
  wsgroup[grepl(pattern, wsgroup, ...)]
}
