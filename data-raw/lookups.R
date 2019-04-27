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
fwa_lookup_layer <- c("stream-network", "coastlines",
                      "watersheds", "watershed-groups")

###### ------ streams
layers <- st_layers(dsn_st)$name[1:246]

stream <- map_dfr(layers, ~ st_read(dsn = dsn_st, layer = .,
                                    query = paste("select FWA_WATERSHED_CODE,
                                    WATERSHED_KEY, GNIS_NAME, WATERSHED_GROUP_CODE from", .)) %>%
                    st_set_geometry(NULL)) %>%
  distinct()

stream$FWA_WATERSHED_CODE %<>% as.character()
stream$GNIS_NAME %<>% as.character()

###### ------ coastline
coastline <- st_read(dsn_bc,
                     layer = "FWA_COASTLINES_SP",
                     query = "select WATERSHED_KEY, FWA_WATERSHED_CODE,
                     WATERSHED_GROUP_CODE from FWA_COASTLINES_SP") %>%
  st_set_geometry(NULL) %>%
  distinct()

coastline$WATERSHED_GROUP_CODE %<>% as.character()
coastline$FWA_WATERSHED_CODE %<>% as.character()

###### ------ watershed groups
wsgroup <- st_read(dsn_bc,
                   layer = "FWA_WATERSHED_GROUPS_POLY",
                   query = "select WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME
                   from FWA_WATERSHED_GROUPS_POLY")

wsgroup <- wsgroup %>%
  st_set_geometry(NULL)

wsgroup$WATERSHED_GROUP_CODE %<>% as.character()
wsgroup$WATERSHED_GROUP_NAME %<>% as.character()

###### ------ watershed
layers <- st_layers(dsn_wsp)$name

watershed <- map_dfr(layers, ~ st_read(dsn = dsn_wsp,
                                                  layer = .,
                                                  query = paste("select WATERSHED_KEY,
                                                  FWA_WATERSHED_CODE, WATERSHED_GROUP_CODE from", .)) %>%
                                  st_set_geometry(NULL)) %>%
  distinct()

lookup_gnis <- stream %>%
  select(WATERSHED_KEY, GNIS_NAME) %>%
  distinct() %>%
  filter(!is.na(GNIS_NAME))

fwa_lookup_gnis <- lookup_gnis
use_data(fwa_lookup_gnis, overwrite = TRUE)

lookup_wsgroup <- wsgroup

select_key <- function(x) x %>% select(WATERSHED_KEY, FWA_WATERSHED_CODE, WATERSHED_GROUP_CODE)
lookup_wskey <- bind_rows(
  select_key(stream),
  select_key(watershed),
  select_key(coastline)) %>%
  distinct()

use_data(lookup_gnis, lookup_wskey, lookup_wsgroup, internal = TRUE, overwrite = TRUE)
