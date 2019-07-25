library(dplyr)
library(sf)
library(purrr)
library(magrittr)
library(checkr)
library(tidyr)
library(usethis)

dsn_bc <- "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb/"
dsn_wsl <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHED_BOUNDARIES_SP.gdb/"
dsn_wsp <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb/"
dsn_lb <- "~/Poisson/Data/spatial/fwa/gdb/FWA_LINEAR_BOUNDARIES_SP.gdb/"
dsn_st <- "~/Poisson/Data/spatial/fwa/gdb/FWA_STREAM_NETWORKS_SP.gdb/"

###### ------ layers
layer_names <- function(x){
  y <- bcdata::bcdc_query_geodata(paste0("freshwater-atlas-", x))
  n <- y$obj$details$column_name
  tibble(
    layer = x,
    WATERSHED_KEY = "WATERSHED_KEY" %in% n,
         WATERSHED_GROUP_CODE = "WATERSHED_GROUP_CODE" %in% n,
    GNIS_NAME = "GNIS_NAME" %in% n || "GNIS_NAME_1" %in% n,
         GNIS_NAME_ = "GNIS_NAME_1" %in% n,
         FWA_WATERSHED_CODE = "FWA_WATERSHED_CODE" %in% n,
    STREAM_ORDER = "STREAM_ORDER" %in% n)
}

layers <- c("stream-network", "coastlines",
          "watersheds", "named-watersheds",
          "manmade-waterbodies",
          "obstructions", "linear-boundaries",
          "lakes", "rivers", "wetlands",
          "watershed-groups", "glaciers")

lookup_layer <- do.call("rbind", lapply(layers, layer_names))

# glaciers and watersheds have so few GNIS it is not worth providing filter option
lookup_layer$GNIS_NAME_[lookup_layer$layer == "watersheds"] <- FALSE
lookup_layer$GNIS_NAME[lookup_layer$layer == "watersheds"] <- FALSE

lookup_layer$GNIS_NAME_[lookup_layer$layer == "glaciers"] <- FALSE
lookup_layer$GNIS_NAME[lookup_layer$layer == "glaciers"] <- FALSE

fwa_lookup_layer <- lookup_layer
use_data(fwa_lookup_layer, overwrite = TRUE)

###### ------ records
tmp <- bcdata::bcdc_search("freshwater-atlas-")
records <- names(tmp)[grepl("freshwater-atlas-", names(tmp))]
name <- gsub("freshwater-atlas-", "", records)
lookup_record <- lapply(records, function(x) tmp[[x]]$id)
names(lookup_record) <- name

###### ------ streams
layers <- st_layers(dsn_st)$name[1:246]

get_gnis <- function(layers, dsn, colname){
  if("GNIS_NAME_1" %in% colname){
  x <- map_dfr(layers, function(x){
    query <- paste0("select ", paste(colname, collapse = ", "),
                    " from ", x, " where GNIS_NAME_1 IS NOT NULL")
    st_read(dsn = dsn, layer = x, query = query) %>%
      st_set_geometry(NULL)
  }) %>%
    gather(tmp, GNIS_NAME, GNIS_NAME_1, GNIS_NAME_2, GNIS_NAME_3) %>%
    select(-tmp) %>%
    filter(!is.na(GNIS_NAME)) %>%
    distinct()
  } else {
    x <- map_dfr(layers, function(x){
      query <- paste0("select ", paste(colname, collapse = ", "),
                      " from ", x, " where GNIS_NAME IS NOT NULL")
      st_read(dsn = dsn, layer = x, query = query) %>%
        st_set_geometry(NULL)
  }) %>%
    distinct()
  }
  x
}

get_wskey <- function(layers, dsn, colname){
  map_dfr(layers, function(x){
    query <- paste0("select ", paste(colname, collapse = ", "),
                    " from ", x)
    st_read(dsn = dsn, layer = x, query = query) %>%
      st_set_geometry(NULL)
  }) %>% distinct()
}

gniscols1 <- c("GNIS_NAME", "FWA_WATERSHED_CODE", "WATERSHED_GROUP_CODE")
gniscols2 <- c("GNIS_NAME_1", "GNIS_NAME_2", "GNIS_NAME_3",
               "FWA_WATERSHED_CODE", "WATERSHED_GROUP_CODE")
gniscols3 <- c("GNIS_NAME", "FWA_WATERSHED_CODE")

wscols1 <- c("WATERSHED_KEY", "FWA_WATERSHED_CODE", "WATERSHED_GROUP_CODE")
wscols2 <- c("WATERSHED_KEY", "FWA_WATERSHED_CODE")

###### ------ stream network
stream_gnis <- get_gnis(layers, dsn_st, gniscols1)
stream_gnis$`stream-network` <- TRUE

stream_wskey <- get_wskey(layers, dsn_st, wscols1)
stream_wskey$`stream-network` <- TRUE

###### ------ linear boundaries
layers <- st_layers(dsn_lb)$name

# no gnis
linear_wskey <- get_wskey(layers, dsn_lb, wscols1)
linear_wskey$`linear-boundaries` <- TRUE

###### ------ watersheds
layers <- st_layers(dsn_wsp)$name

watersheds_wskey <- get_wskey(layers, dsn_wsp, wscols1)
watersheds_wskey$`watersheds` <- TRUE

###### ------ coastline
coastlines_wskey <- get_wskey("FWA_COASTLINES_SP", dsn_bc, wscols1)
coastlines_wskey$coastlines <- TRUE

###### ------ obstructions
obstructions_gnis <- get_gnis("FWA_OBSTRUCTIONS_SP", dsn_bc, gniscols1)
obstructions_gnis$`obstructions` <- TRUE

obstructions_wskey <- get_wskey("FWA_OBSTRUCTIONS_SP", dsn_bc, wscols1)
obstructions_wskey$`obstructions` <- TRUE

###### ------ lakes
lakes_gnis <- get_gnis("FWA_LAKES_POLY", dsn_bc, gniscols2)
lakes_gnis$lakes <- TRUE

lakes_wskey <- get_wskey("FWA_LAKES_POLY", dsn_bc, wscols1)
lakes_wskey$lakes <- TRUE

###### ------ rivers
rivers_gnis <- get_gnis("FWA_RIVERS_POLY", dsn_bc, gniscols2)
rivers_gnis$rivers <- TRUE

rivers_wskey <- get_wskey("FWA_RIVERS_POLY", dsn_bc, wscols1)
rivers_wskey$rivers <- TRUE

###### ------ wetlands
wetlands_gnis <- get_gnis("FWA_WETLANDS_POLY", dsn_bc, gniscols2)
wetlands_gnis$wetlands <- TRUE

wetlands_wskey <- get_wskey("FWA_WETLANDS_POLY", dsn_bc, wscols1)
wetlands_wskey$wetlands <- TRUE

###### ------ manmade waterbodies
manmade_gnis <- get_gnis("FWA_MANMADE_WATERBODIES_POLY", dsn_bc, gniscols2)
manmade_gnis$manmade <- TRUE

manmade_wskey <- get_wskey("FWA_MANMADE_WATERBODIES_POLY", dsn_bc, wscols1)
manmade_wskey$manmade <- TRUE

###### ------ glaciers
glaciers_wskey <- get_wskey("FWA_GLACIERS_POLY", dsn_bc, wscols1)
glaciers_wskey$glaciers <- TRUE

###### ------ watershed groups
watershed_group <- st_read(dsn_bc, layer = "FWA_WATERSHED_GROUPS_POLY")
watershed_group$`watershed-groups` <- TRUE

###### ------ named watersheds
named_gnis <- get_gnis("FWA_NAMED_WATERSHEDS_POLY", dsn_bc, gniscols3)
named_gnis$`named-watersheds` <- TRUE

named_wskey <- get_wskey("FWA_NAMED_WATERSHEDS_POLY", dsn_bc, wscols2)
named_wskey$`named-watersheds` <- TRUE

###### ------ combine lookup
lookup_gnis <- stream_gnis %>%
  full_join(rivers_gnis, c("FWA_WATERSHED_CODE", "GNIS_NAME", "WATERSHED_GROUP_CODE")) %>%
  full_join(manmade_gnis, c("FWA_WATERSHED_CODE", "GNIS_NAME", "WATERSHED_GROUP_CODE")) %>%
  full_join(obstructions_gnis, c("FWA_WATERSHED_CODE", "GNIS_NAME", "WATERSHED_GROUP_CODE")) %>%
  full_join(lakes_gnis, c("FWA_WATERSHED_CODE", "GNIS_NAME", "WATERSHED_GROUP_CODE")) %>%
  full_join(wetlands_gnis, c("FWA_WATERSHED_CODE", "GNIS_NAME", "WATERSHED_GROUP_CODE")) %>%
  full_join(named_gnis, c("FWA_WATERSHED_CODE", "GNIS_NAME"))

lookup_gnis %<>% modify_if(.p = ~ is.logical(.), .f = ~ replace_na(., FALSE))

lookup_wskey <- stream_wskey %>%
  full_join(coastlines_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(watersheds_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(manmade_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(obstructions_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(linear_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(lakes_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(rivers_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(wetlands_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(glaciers_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY", "WATERSHED_GROUP_CODE")) %>%
  full_join(named_wskey, c("FWA_WATERSHED_CODE", "WATERSHED_KEY"))

lookup_wskey %<>% modify_if(.p = ~ is.logical(.), .f = ~ replace_na(., FALSE))

###### ------ watershed group lookup
lookup_wsgroup <- lookup_wskey %>%
  select(WATERSHED_GROUP_CODE, `stream-network`:`named-watersheds`) %>%
  filter(!is.na(WATERSHED_GROUP_CODE)) %>%
  group_by(WATERSHED_GROUP_CODE) %>%
  summarise(`stream-network` = ifelse(any(`stream-network`), TRUE, FALSE),
            `coastlines` = ifelse(any(`coastlines`), TRUE, FALSE),
            `watersheds` = ifelse(any(`watersheds`), TRUE, FALSE),
            `manmade` = ifelse(any(`manmade`), TRUE, FALSE),
            `obstructions` = ifelse(any(`obstructions`), TRUE, FALSE),
            `linear-boundaries` = ifelse(any(`linear-boundaries`), TRUE, FALSE),
            `lakes` = ifelse(any(`lakes`), TRUE, FALSE),
            `rivers` = ifelse(any(`rivers`), TRUE, FALSE),
            `wetlands` = ifelse(any(`wetlands`), TRUE, FALSE),
            `glaciers` = ifelse(any(`glaciers`), TRUE, FALSE),
            `named-watersheds` = ifelse(any(`named-watersheds`), TRUE, FALSE))


watershed_group$WATERSHED_GROUP_CODE %<>% as.character()
watershed_group$WATERSHED_GROUP_NAME %<>% as.character()

lookup_wsgroup <- watershed_group %>%
  select(WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME) %>%
  st_set_geometry(NULL) %>%
  right_join(lookup_wsgroup, "WATERSHED_GROUP_CODE")

fwa_lookup_watershed_group <- lookup_wsgroup
fwa_lookup_gnis <- lookup_gnis

use_data(fwa_lookup_watershed_group, overwrite = TRUE)
use_data(fwa_lookup_gnis, overwrite = TRUE)

use_data(lookup_gnis, lookup_wskey, lookup_wsgroup, lookup_layer, lookup_record, internal = TRUE, overwrite = TRUE)
