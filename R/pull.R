#' Pull WATERSHED_KEY from GNIS_NAME.
#'
#' @param x A vector of valid GNIS_NAME.
#' @return A character vector.
#' @examples
#' fwa_pull_stream_wskey("Sangan River")
#' @export
fwa_pull_wskey <- function(x){
  check_stream_gnis(x)
  lookup_gnis$WATERSHED_KEY[lookup_gnis$GNIS_NAME %in% x]
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

#' Pull tributaries from WATERSHED_KEY.
#'
#' @param x A vector of WATERSHED_KEY.
#' @param order An integer specifying order of tributaries to retrieve,
#' e.g. order = 1L will retrieve all primary upstream tributaries.
#' @return A vector of WATERSHED_KEY tributaries.
#' @examples
#' fwa_pull_tributaries("Sangan River")
#' @export
fwa_pull_tributaries <- function(x){
  check_wskey(x)
  x <- as.character(x)
  a <- gsub("-000000", "", x)
  b <- fwa_lookup_stream_blkey$WatershedCode[grepl(a, fwa_lookup_stream_blkey$WatershedCode, fixed = TRUE)]
  c <- gsub("-000000", "", b) %>% gsub(paste0(a, "-"), "", .)
  d <- c(x, b[sapply(strsplit(c, "-"), function(x) length(x) <= order)])
  fwa_lookup_stream_blkey$BlueLineKey[fwa_lookup_stream_blkey$WatershedCode %in% d]
}

tribs_streams <- function(x, n){
  unlist(lapply(x, function(x){tribs_stream(x, n)}))
}

tribs_stream <- function(x, n){

}

