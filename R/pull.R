#' Pull WATERSHED_KEY from GNIS_NAME.
#'
#' @param x A vector of valid GNIS_NAME.
#' @return A character vector.
#' @examples
#' fwa_pull_watershed_key("Sangan River")
#' @export
fwa_pull_watershed_key <- function(x){
  check_gnis(x)
  lookup_gnis$WATERSHED_KEY[lookup_gnis$GNIS_NAME %in% x]
}

#' Pull WATERSHED_GROUP_CODE from WATERSHED_GROUP_NAME.
#'
#' @param x A vector of valid WATERSHED_GROUP_NAME.
#' @return A character vector.
#' @examples
#' fwa_pull_watershed_group_code("Graham Island")
#' @export
fwa_pull_watershed_group_code <- function(x){
  check_wsgname(x)
  fwa_lookup_watershedgroup$WATERSHED_GROUP_CODE[fwa_lookup_watershedgroup$WATERSHED_GROUP_NAME %in% x]
}

#' Pull tributaries from WATERSHED_KEY.
#'
#' @param x A vector of WATERSHED_KEY.
#' @param order An integer of the tributary order,
#' e.g. order = 1L will retrieve all primary upstream tributaries.
#' @return A vector of WATERSHED_KEY tributaries.
#' @examples
#' fwa_pull_tributaries("Sangan River")
#' @export
fwa_pull_tributaries <- function(x){
  check_wskey(x)
  x <- wskey_to_wscode(x)
  unlist(lapply(x, function(x){tribs(x, order)}))
}

