read_layer <- function(what, layer, x, named_only,
                       tributaries, multiple_gnis, crs){
  x <- switch(what,
              "GNIS_NAME" = read_layer_gnis(layer = layer,
                                            x = x,
                                            named_only = named_only,
                                            tributaries = tributaries,
                                            multiple_gnis = multiple_gnis,
                                            crs = crs),
              "WATERSHED_GROUP_CODE" = read_layer_wsgcode(layer = layer,
                                                          x = x,
                                                          named_only = named_only,
                                                          multiple_gnis = multiple_gnis,
                                                          crs = crs),
              "WATERSHED_KEY" = read_layer_wskey(layer = layer,
                                                 x = x,
                                                 named_only = named_only,
                                                 tributaries = tributaries,
                                                 multiple_gnis = multiple_gnis,
                                                 crs = crs),
              "NULL" = read_layer_null(layer = layer,
                                       x = x,
                                       named_only = named_only,
                                       multiple_gnis = multiple_gnis,
                                       crs = crs),
              NULL)
}

read_layer_gnis <- function(layer, x, named_only,
                            tributaries, multiple_gnis, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)

  if(tributaries){
    cql <- paste0("FWA_WATERSHED_CODE LIKE '", gnis_to_wscode(x), "-%'")
    y <- bcdata::filter(CQL(cql))
  } else if(multiple_gnis){
    y <- bcdata::filter(GNIS_NAME_1 == x | GNIS_NAME_2 == x | GNIS_NAME_3 == x)
  } else {
    y <- bcdata::filter(GNIS_NAME == x)
  }

  if(named_only){
    if(multiple_gnis){
      return(y %>% filter(!is.na(GNIS_NAME_1)))
    } else {
      return(y %>% filter(!is.na(GNIS_NAME)))
    }
  }
  y
}

read_layer_wskey <- function(layer, x, named_only,
                             tributaries, multiple_gnis, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)

  if(tributaries){
    cql <- paste0("FWA_WATERSHED_CODE LIKE '", wskey_to_wscode(x), "-%'")
    y <- bcdata::filter(CQL(cql))
  } else {
    y <- bcdata::filter(WATERSHED_KEY == x)
  }

  if(named_only){
    if(multiple_gnis){
      return(y %>% filter(!is.na(GNIS_NAME_1)))
    } else {
      return(y %>% filter(!is.na(GNIS_NAME)))
    }
  }
  y
}

read_layer_wsgcode <- function(layer, x, named_only,
                               multiple_gnis, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs) %>%
    bcdata::filter(WATERSHED_GROUP_CODE %in% x)
  if(named_only){
    if(multiple_gnis){
      return(y %>% filter(!is.na(GNIS_NAME_1)))
    } else {
      return(y %>% filter(!is.na(GNIS_NAME)))
    }
  }
  y
}

read_layer_null <- function(layer, x, named_only,
                            multiple_gnis, crs){
  y <- bcdata::bcdc_query_geodata(lookup_record[[layer]], crs = crs)
  if(named_only){
    if(multiple_gnis){
      return(y %>% filter(!is.na(GNIS_NAME_1)))
    } else {
      return(y %>% filter(!is.na(GNIS_NAME)))
    }
  }
  y
}
