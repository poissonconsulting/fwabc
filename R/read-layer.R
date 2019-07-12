read_layer <- function(what, layer, x,
                       named_only, tributaries,
                       min_stream_order, crs){
  x <- switch(what,
              "GNIS_NAME" = read_layer_gnis(layer, x,
                                            tributaries, min_stream_order, crs),
              "WATERSHED_GROUP_CODE" = read_layer_wsgcode(layer, x, named_only,
                                                          min_stream_order, crs),
              "WATERSHED_KEY" = read_layer_wskey(layer, x, named_only,
                                                 tributaries, min_stream_order, crs),
              "NULL" = read_layer_null(layer, x, named_only,
                                       min_stream_order, crs),
              NULL)
}

read_layer_gnis <- function(layer, x, tributaries,
                            min_stream_order, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)
  if(tributaries){
    code <- gnis_to_wscode(x)
    return(y %>% bcdata::filter(CQL(glue("FWA_WATERSHED_CODE LIKE '{code}%'"))) %>%
             bcdata::filter(STREAM_ORDER >= min_stream_order))
  }
  y %>% bcdata::filter(GNIS_NAME %in% x,
                       STREAM_ORDER >= min_stream_order)
}

read_layer_wskey <- function(layer, x, named_only, tributaries,
                            min_stream_order, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)
  if(named_only && tributaries){
    code <- wskey_to_wscode(x)
    return(y %>% bcdata::filter(CQL(glue("FWA_WATERSHED_CODE LIKE '{code}%'"))) %>%
             bcdata::filter(STREAM_ORDER >= min_stream_order,
                            !is.na(GNIS_NAME)))
  }
  if(named_only && !tributaries){
    return(y %>% bcdata::filter(WATERSHED_KEY %in% x,
                                STREAM_ORDER >= min_stream_order,
                                !is.na(GNIS_NAME)))

  }
  y %>% bcdata::filter(WATERSHED_KEY %in% x,
                       STREAM_ORDER >= min_stream_order)
}

read_layer_wsgcode <- function(layer, x, named_only,
                            min_stream_order, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)
  if(named_only){
    return(y %>% bcdata::filter(WATERSHED_GROUP_CODE %in% x,
                                STREAM_ORDER >= min_stream_order,
                                !is.na(GNIS_NAME)))
  }
  y %>% bcdata::filter(WATERSHED_GROUP_CODE %in% x,
                       STREAM_ORDER >= min_stream_order)
}

read_layer_null <- function(layer, x, named_only,
                            min_stream_order, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)
  if(named_only){
    return(y %>% bcdata::filter(STREAM_ORDER >= min_stream_order,
                                !is.na(GNIS_NAME)))
  }
  y %>% bcdata::filter(STREAM_ORDER >= min_stream_order)
}
