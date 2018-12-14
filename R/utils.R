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


stream_to_blk <- function(x){
  unlist(lapply(x, function(x){
    if(is_blk_stream(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(fwa_lookup_stream_gnis$BlueLineKey[fwa_lookup_stream_gnis$GnisName %in% x])
    }
    fwa_lookup_stream_blkey$BlueLineKey[fwa_lookup_stream_blkey$WatershedCode %in% x]
  }))
}

stream_to_wscode <- function(x){
  unlist(lapply(x, function(x){
    if(is_ws_code_stream(x)){
      return(x)
    }
    if(is_gnis(x)){
      return(fwa_lookup_stream_gnis$WatershedCode[fwa_lookup_stream_gnis$GnisName %in% x])
    }
    fwa_lookup_stream_blkey$WatershedCode[fwa_lookup_stream_blkey$BlueLineKey %in% x]
  }))
}

wsgname_to_wsgcode <- function(x){
  unlist(lapply(x, function(x){
    if(!(is_wsg_name(x))){
      return(x)
    }
    fwa_lookup_wsgroup$WatershedGroupCode[fwa_lookup_wsgroup$WatershedGroupName %in% x]
  }))
}

tribs_stream <- function(x){
  x <- gsub("-000000", "", x)
  unlist(lapply(x, function(x){
    fwa_lookup_stream_blkey$BlueLineKey[grepl(x, fwa_lookup_stream_blkey$WatershedCode, fixed = TRUE)]
  }))
}

tribs_wshed <- function(x){
  x <- gsub("-000000", "", x)
  unlist(lapply(x, function(x){
    fwa_lookup_watershed$WatershedCode[grepl(x, fwa_lookup_watershed$WatershedCode, fixed = TRUE)]
  }))
}



line_sample <- function(data, distance){
  # data <- try(st_cast(data, "LINESTRING", silent = TRUE))
  # if(inherits(data, "try-error")) ps_error("data cannot be cast to LINESTRING")
  sample <- seq(0, 1, 1/as.vector(round(st_length(data)/distance)))

  data %>%
    st_line_sample(sample = sample)  %>%
    st_cast("POINT") %>%
    st_sf()
}

