#' Read layer from GNIS_NAME, WATERSHED_KEY, WATERSHED_GROUP_NAME or WATERSHED_GROUP_CODE
#'
#' @param x A vector of valid GNIS_NAME, WATERSHED_KEY, WATERSHED_GROUP_NAME or WATERSHED_GROUP_CODE.
#' If NULL, entire dataset is read.
#' @param named_only A flag indicating whether to only include features with GNIS_NAME.
#' @param tributaries A flag indicating whether to read all tributaries in addition
#'  (only applicable if x is GNIS_NAME or WATERSHED_KEY).
#' @param crs The epsg code for the coordinate reference system. Defaults to `3005`
#'        (B.C. Albers). See https://epsgi.io.
#' @param collect A flag indicating whether to collect result.
#' @param check A flag indicating whether to check that x is valid.
#' @param input_type A character string of the column name to filter x by.
#' If NULL, input_type is guessed. This is useful if input is
#' both GNIS_NAME and WATERSHED_GROUP_NAME, e.g. 'Skagit River'.
read_gkcn <- function(...){
  read(types = "gkcn", ...)
}

#' Read layer from GNIS_NAME or WATERSHED_KEY
#'
#' @param x A vector of valid GNIS_NAME or WATERSHED_KEY.
#' If NULL, entire dataset is read.
#' @inheritParams read_gkcn
read_gk <- function(...){
  read(types = "gk", ...)
}

#' Read layer from WATERSHED_KEY, WATERSHED_GROUP_NAME or WATERSHED_GROUP_CODE
#'
#' @param x A vector of valid WATERSHED_KEY, WATERSHED_GROUP_NAME or WATERSHED_GROUP_CODE.
#' If NULL, entire dataset is read.
#' @inheritParams read_gkcn
read_kcn <- function(...){
  read(types = "kcn", ...)
}

#' Read layer from WATERSHED_GROUP_NAME or WATERSHED_GROUP_CODE
#'
#' @param x A vector of valid WATERSHED_GROUP_NAME or WATERSHED_GROUP_CODE.
#' If NULL, entire dataset is read.
#' @inheritParams read_gkcn
read_cn <- function(...){
  read(types = "cn", ...)
}

read <- function(types, layer, x, named_only = FALSE,
                 tributaries, crs, collect,
                 check, input_type){

  check_flag(check)
  check_flag(named_only)
  check_flag(tributaries)
  check_numeric(crs)
  check_input_type(input_type)

  what <- input_type
  if(is.null(input_type))
    what <- what_is_it(x[1])

  if(check){
    if(!is.null(what)){
      check_x(x, types)
      check_x_layer(x, what, layer)
    }
  }

  if(what == "WATERSHED_GROUP_NAME"){
    what <- "WATERSHED_GROUP_CODE"
    x <- wsgname_to_wsgcode(x)
  }
  print(x)
  print(what)

  x <- switch(what,
              "GNIS_NAME" = read_gnis(layer = layer,
                                            x = x,
                                            named_only = named_only,
                                            tributaries = tributaries,
                                            crs = crs),
              "WATERSHED_GROUP_CODE" = read_wsgcode(layer = layer,
                                                          x = x,
                                                          named_only = named_only,
                                                          crs = crs),
              "WATERSHED_KEY" = read_wskey(layer = layer,
                                                 x = x,
                                                 named_only = named_only,
                                                 tributaries = tributaries,
                                                 crs = crs),
              "NULL" = read_null(layer = layer,
                                       x = x,
                                       named_only = named_only,
                                       multiple_gnis = multiple_gnis,
                                       crs = crs),
              NULL)
  if(collect)
    return(x %>% bcdata::collect())
  x
}

read_gnis <- function(layer, x, named_only, tributaries, crs){

  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)
  multiple_gnis <- lookup_layer$GNIS_NAME_[lookup_layer$layer == layer]

  if(tributaries){
    cql <- paste0("FWA_WATERSHED_CODE LIKE '", gnis_to_wscode(x, layer), "-%'")
    y <- y %>% bcdata::filter(CQL(cql))
  } else if(multiple_gnis){
    y <- y %>% bcdata::filter(GNIS_NAME_1 == x | GNIS_NAME_2 == x | GNIS_NAME_3 == x)
  } else {
    y <- y %>% bcdata::filter(GNIS_NAME == x)
  }

  if(named_only){
    if(multiple_gnis){
      return(y %>% bcdata::filter(!is.na(GNIS_NAME_1)))
    } else {
      return(y %>% bcdata::filter(!is.na(GNIS_NAME)))
    }
  }
  y
}

read_wskey <- function(layer, x, named_only, tributaries, crs){

  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)
  multiple_gnis <- lookup_layer$GNIS_NAME_[lookup_layer$layer == layer]

  if(tributaries){
    cql <- paste0("FWA_WATERSHED_CODE LIKE '", wskey_to_wscode(x, layer), "-%'")
    y <- y %>% bcdata::filter(CQL(cql))
  } else {
    y <- y %>% bcdata::filter(WATERSHED_KEY == x)
  }

  if(named_only){
    if(multiple_gnis){
      return(y %>% bcdata::filter(!is.na(GNIS_NAME_1)))
    } else {
      return(y %>% bcdata::filter(!is.na(GNIS_NAME)))
    }
  }
  y
}

read_wsgcode <- function(layer, x, named_only, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs) %>%
    bcdata::filter(WATERSHED_GROUP_CODE %in% x)
  multiple_gnis <- lookup_layer$GNIS_NAME_[lookup_layer$layer == layer]

  if(named_only){
    if(multiple_gnis){
      return(y %>% bcdata::filter(!is.na(GNIS_NAME_1)))
    } else {
      return(y %>% bcdata::filter(!is.na(GNIS_NAME)))
    }
  }
  y
}

read_null <- function(layer, x, named_only, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)
  multiple_gnis <- lookup_layer$GNIS_NAME_[lookup_layer$layer == layer]

  if(named_only){
    if(multiple_gnis){
      return(y %>% filter(!is.na(GNIS_NAME_1)))
    } else {
      return(y %>% filter(!is.na(GNIS_NAME)))
    }
  }
  y
}
