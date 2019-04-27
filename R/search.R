#' Search valid GNIS_NAME.
#'
#' @param pattern A character string containing a regular expression (or character string for fixed = TRUE) to be matched.
#' @param ignore_case A flag indicating whether to ignore case.
#' @param ... Arguments passed to grepl.
#' @return A character vector.
#' @examples
#' fwa_search_gnis_name("sangan")
#' @export
fwa_search_gnis_name <- function(pattern, ignore_case = TRUE, ...){
  lookup_gnis$GNIS_NAME[which(grepl(pattern, lookup_gnis$GNIS_NAME, ignore.case = ignore_case, ...))]
}

#' Search valid WATERSHED_GROUP_NAME.
#'
#' @param pattern A character string containing a regular expression (or character string for fixed = TRUE) to be matched.
#' @param ignore_case A flag indicating whether to ignore case.
#' @param ... Arguments passed to grepl.
#' @return A character vector.
#' @examples
#' fwa_search_watershed_group("graham")
#' @export
fwa_search_watershed_group <- function(pattern, ignore_case = TRUE, ...){
  lookup_wsgroup$WATERSHED_GROUP_NAME[which(grepl(pattern, lookup_wsgroup$WATERSHED_GROUP_NAME, ignore.case = ignore_case, ...))]
}
