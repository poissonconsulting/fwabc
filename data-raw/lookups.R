library(dplyr)
library(sf)
library(purrr)
library(magrittr)
library(checkr)
library(tidyr)

dsn_bc <- "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb/"
dsn_wsl <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHED_BOUNDARIES_SP.gdb/"
dsn_wsp <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb/"
dsn_lb <- "~/Poisson/Data/spatial/fwa/gdb/FWA_LINEAR_BOUNDARIES_SP.gdb/"
dsn_st <- "~/Poisson/Data/spatial/fwa/gdb/FWA_STREAM_NETWORKS_SP.gdb/"

###### ------ layers and metadata
fwa_metadata <- list(FWA_BC = st_layers(dsn_bc),
                     FWA_STREAM_NETWORKS_SP = st_layers(dsn_st),
                     # FWA_WATERSHED_BOUNDARIES_SP = st_layers(dsn_wsl),
                     # FWA_LINEAR_BOUNDARIES_SP = st_layers(dsn_lb),
                     FWA_WATERSHEDS_POLY = st_layers(dsn_wsp))

fwa_layers <- lapply(fwa_metadata, function(x) x$name[!is.na(x$geomtype)])
fwa_layers <- map_dfr(names(fwa_metadata), function(x){
  data <- fwa_metadata[x]
  tibble(db = x, layer = data[[1]]$name)
})

usethis::use_data(fwa_metadata, overwrite = TRUE)
usethis::use_data(fwa_layers, overwrite = TRUE)

###### ------ streams
layers <- st_layers(dsn_st)$name[1:246]

stream <- map_dfr(layers, ~ st_read(dsn = dsn_st, layer = .,
                                    query = paste("select FWA_WATERSHED_CODE,
                                    WATERSHED_KEY, GNIS_NAME, WATERSHED_GROUP_CODE from", .)) %>%
                                  st_set_geometry(NULL)) %>%
  distinct()

stream$FWA_WATERSHED_CODE %<>% as.character()
stream$GNIS_NAME %<>% as.character()

fwa_lookup_stream <- stream
usethis::use_data(fwa_lookup_stream, overwrite = TRUE)

###### ------ coastline
coastline <- st_read(dsn_bc,
                     layer = "FWA_COASTLINES_SP",
                     query = "select WATERSHED_KEY, FWA_WATERSHED_CODE,
                     WATERSHED_GROUP_CODE from FWA_COASTLINES_SP") %>%
  st_set_geometry(NULL) %>%
  distinct()

coastline$WATERSHED_GROUP_CODE %<>% as.character()

fwa_lookup_coastline <- coastline
usethis::use_data(fwa_lookup_coastline, overwrite = TRUE)

###### ------ watershed groups
wsgroup <- st_read(dsn_bc,
                   layer = "FWA_WATERSHED_GROUPS_POLY",
                   query = "select WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME
                   from FWA_WATERSHED_GROUPS_POLY")

wsgroup <- wsgroup %>%
  st_set_geometry(NULL)

wsgroup$WATERSHED_GROUP_CODE %<>% as.character()
wsgroup$WATERSHED_GROUP_NAME %<>% as.character()

fwa_lookup_watershedgroup <- wsgroup
usethis::use_data(fwa_lookup_watershedgroup, overwrite = TRUE)

###### ------ watershed
layers <- st_layers(dsn_wsp)$name

fwa_lookup_watershed <- map_dfr(layers, ~ st_read(dsn = dsn_wsp,
                                                  layer = .,
                                                  query = paste("select WATERSHED_KEY,
                                                  FWA_WATERSHED_CODE, WATERSHED_GROUP_CODE from", .)) %>%
                                          st_set_geometry(NULL)) %>%
  distinct()

gnis <- stream %>%
  select(WATERSHED_KEY, GNIS_NAME) %>%
  distinct() %>%
  filter(!is.na(GNIS_NAME))

fwa_lookup_watershed <- fwa_lookup_watershed %>%
  left_join(gnis, "WATERSHED_KEY")

usethis::use_data(fwa_lookup_watershed, overwrite = TRUE)

