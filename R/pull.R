#' Pull WATERSHED_KEY from stream GNIS_NAME.
#'
#' @param x A vector of valid stream GNIS_NAME.
#' @return A character vector.
#' @examples
#' fwa_pull_stream_wskey("Sangan River")
#' @export
fwa_pull_stream_wskey <- function(x){
  check_stream_gnis(x)
  lookup_stream_gnis$WATERSHED_KEY[lookup_stream_gnis$GNIS_NAME %in% x]
}

#' Pull WATERSHED_KEY from watershed GNIS_NAME.
#'
#' @param x A vector of valid watershed GNIS_NAME.
#' @return A character vector.
#' @examples
#' fwa_pull_wshed_wskey("Sangan River")
#' @export
fwa_pull_wshed_wskey <- function(x){
  check_wshed_gnis(x)
  lookup_wshed_gnis$WATERSHED_KEY[lookup_wshed_gnis$GNIS_NAME %in% x]
}

#' Pull WATERSHED_GROUP_CODE from WATERSHED_GROUP_NAME.
#'
#' @param x A vector of valid WATERSHED_GROUP_NAME.
#' @return A character vector.
#' @examples
#' fwa_pull_wsgcode("Graham Island")
#' @export
fwa_pull_wsgcode <- function(x){
  check_wsgcode(x)
  fwa_lookup_watershedgroup$WATERSHED_GROUP_NAME[fwa_lookup_watershedgroup$WATERSHED_GROUP_CODE %in% x]
}

