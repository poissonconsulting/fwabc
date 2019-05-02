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
  lookup_wsgroup$WATERSHED_GROUP_CODE[lookup_wsgroup$WATERSHED_GROUP_NAME %in% x]
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
fwa_pull_tributaries <- function(x, order = 1){
  check_wskey(x)
  order <- as.integer(order)
  check_vector(order, 1L, length = 1)

  x <- wskey_to_wscode(x)
  unlist(lapply(x, function(x){tribs(x, order)}))
}

