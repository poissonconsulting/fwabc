#' Pull BLUE_LINE_KEY from GNIS_NAME.
#'
#' @param x A vector of valid GNIS_NAME.
#' @return A character vector.
#' @examples
#' fwa_pull_bluelinekey("Sangan River")
#' @export
fwa_pull_bluelinekey <- function(x){
  check_gnis(x)
  lookup_stream_gnis$BLUE_LINE_KEY[lookup_stream_gnis$GNIS_NAME %in% x]
}

#' Pull WATERSHED_KEY from GNIS_NAME.
#'
#' @param x A vector of valid GNIS_NAME.
#' @return A character vector.
#' @examples
#' fwa_pull_watershedkey("Sangan River")
#' @export
fwa_pull_watershedkey <- function(x){
  check_gnis(x)
  fwa_lookup_gnis$WATERSHED_KEY[fwa_lookup_gnis$GNIS_NAME %in% x]
}

