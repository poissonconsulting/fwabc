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
fwa_lookup_layer <- tibble(layer = c("stream-network", "coastlines",
                      "watersheds", "manmade-waterbodies",
                      "obstructions", "linear-boundaries",
                      "lakes", "rivers", "wetlands",
                      "watershed-groups", "glaciers"),
                      WATERSHED_KEY = c(rep(TRUE, 9), FALSE, FALSE),
                      WATERSHED_GROUP_CODE = TRUE)

use_data(fwa_lookup_layer, overwrite = TRUE)

###### ------ streams
layers <- st_layers(dsn_st)$name[1:246]

stream <- map_dfr(layers, ~ st_read(dsn = dsn_st, layer = .) %>%
                    st_set_geometry(NULL)) %>%
  distinct()

stream$FWA_WATERSHED_CODE %<>% as.character()
stream$GNIS_NAME %<>% as.character()
stream$`stream-network` <- TRUE

###### ------ linear boundaries
layers <- st_layers(dsn_lb)$name

linear <- map_dfr(layers, ~ st_read(dsn = dsn_lb, layer = .) %>%
                    st_set_geometry(NULL)) %>%
  distinct()

linear$FWA_WATERSHED_CODE %<>% as.character()
linear$`linear-boundaries` <- TRUE

###### ------ watershed
layers <- st_layers(dsn_wsp)$name

watershed <- map_dfr(layers, ~ st_read(dsn = dsn_wsp,
                                       layer = .) %>%
                       st_set_geometry(NULL)) %>%
  distinct()
watershed$watersheds <- TRUE

###### ------ coastline
coastline <- st_read(dsn_bc,
                     layer = "FWA_COASTLINES_SP") %>%
  st_set_geometry(NULL) %>%
  distinct()

coastline$WATERSHED_GROUP_CODE %<>% as.character()
coastline$FWA_WATERSHED_CODE %<>% as.character()
coastline$coastlines <- TRUE

###### ------ obstructions
obstruction <- st_read(dsn_bc,
                     layer = "FWA_OBSTRUCTIONS_SP") %>%
  st_set_geometry(NULL) %>%
  distinct()

obstruction$WATERSHED_GROUP_CODE %<>% as.character()
obstruction$FWA_WATERSHED_CODE %<>% as.character()
obstruction$obstructions <- TRUE

###### ------ lakes
lake <- st_read(dsn_bc,
                     layer = "FWA_LAKES_POLY") %>%
  st_set_geometry(NULL) %>%
  distinct()

lake$WATERSHED_GROUP_CODE %<>% as.character()
lake$FWA_WATERSHED_CODE %<>% as.character()
lake$lakes <- TRUE

###### ------ rivers
river <- st_read(dsn_bc,
                     layer = "FWA_RIVERS_POLY") %>%
  st_set_geometry(NULL) %>%
  distinct()

river$WATERSHED_GROUP_CODE %<>% as.character()
river$FWA_WATERSHED_CODE %<>% as.character()
river$rivers <- TRUE

###### ------ wetlands
wetland <- st_read(dsn_bc,
                     layer = "FWA_WETLANDS_POLY") %>%
  st_set_geometry(NULL) %>%
  distinct()

wetland$WATERSHED_GROUP_CODE %<>% as.character()
wetland$FWA_WATERSHED_CODE %<>% as.character()
wetland$wetlands <- TRUE

###### ------ manmade waterbodies
manmade <- st_read(dsn_bc,
                     layer = "FWA_MANMADE_WATERBODIES_POLY") %>%
  st_set_geometry(NULL) %>%
  distinct()

manmade$WATERSHED_GROUP_CODE %<>% as.character()
manmade$FWA_WATERSHED_CODE %<>% as.character()
manmade$`manmade-waterbodies` <- TRUE

###### ------ glaciers
glacier <- st_read(dsn_bc,
                     layer = "FWA_GLACIERS_POLY") %>%
  st_set_geometry(NULL) %>%
  distinct()

glacier$WATERSHED_GROUP_CODE %<>% as.character()
glacier$FWA_WATERSHED_CODE %<>% as.character()
glacier$glaciers <- TRUE

###### ------ watershed groups
wsgroup <- st_read(dsn_bc,
                   layer = "FWA_WATERSHED_GROUPS_POLY")

wsgroup <- wsgroup %>%
  st_set_geometry(NULL)

wsgroup$WATERSHED_GROUP_CODE %<>% as.character()
wsgroup$WATERSHED_GROUP_NAME %<>% as.character()
wsgroup$`watershed-groups` <- TRUE

###### ------ combine lookup
all_data <- bind_rows(stream,
                    linear,
                    watershed,
                    coastline,
                    obstruction,
                    lake,
                    wetland,
                    river,
                    manmade,
                    glacier)

lookup <- all_data %>%
  select(WATERSHED_KEY, WATERSHED_GROUP_CODE,
         FWA_WATERSHED_CODE, GNIS_NAME,
         GNIS_NAME_1, GNIS_NAME_2, GNIS_NAME_3) %>%
  distinct()

lookup %<>% left_join(stream %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `stream-network`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(linear %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `linear-boundaries`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(watershed %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `watersheds`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(coastline %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `coastlines`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(obstruction %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `obstructions`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(lake %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `lakes`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(wetland %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `wetlands`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(river %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `rivers`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(manmade %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `manmade-waterbodies`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))
lookup %<>% left_join(glacier %>% select(WATERSHED_KEY, WATERSHED_GROUP_CODE, `glaciers`) %>% distinct(), c("WATERSHED_KEY", "WATERSHED_GROUP_CODE"))

lookup %<>% filter(!is.na(WATERSHED_KEY))
lookup %<>% filter(!is.na(FWA_WATERSHED_CODE))

lookup_wskey <- lookup %>%
  select(-GNIS_NAME, -GNIS_NAME_1,
         -GNIS_NAME_2, -GNIS_NAME_3)

lookup_wskey %<>% modify_if(.p = ~ is.logical(.), .f = ~ replace_na(., FALSE))

lookup_gnis <- lookup %>%
  select(-FWA_WATERSHED_CODE, -WATERSHED_GROUP_CODE) %>%
  gather(tmp, GNIS_NAME, GNIS_NAME, GNIS_NAME_1, GNIS_NAME_2, GNIS_NAME_3) %>%
  select(-tmp) %>%
  filter(!is.na(GNIS_NAME)) %>%
  distinct()

lookup_gnis %<>% modify_if(.p = ~ is.logical(.), .f = ~ replace_na(., FALSE))
lookup_gnis %<>% select(GNIS_NAME, WATERSHED_KEY, everything())

fwa_lookup_gnis <- lookup_gnis
use_data(fwa_lookup_gnis, overwrite = TRUE)

lookup <- all_data %>%
  select(WATERSHED_GROUP_CODE) %>%
  distinct()

lookup %<>% left_join(stream %>% select(WATERSHED_GROUP_CODE, `stream-network`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(linear %>% select(WATERSHED_GROUP_CODE, `linear-boundaries`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(watershed %>% select(WATERSHED_GROUP_CODE, `watersheds`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(coastline %>% select(WATERSHED_GROUP_CODE, `coastlines`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(obstruction %>% select(WATERSHED_GROUP_CODE, `obstructions`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(lake %>% select(WATERSHED_GROUP_CODE, `lakes`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(wetland %>% select(WATERSHED_GROUP_CODE, `wetlands`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(river %>% select(WATERSHED_GROUP_CODE, `rivers`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(manmade %>% select(WATERSHED_GROUP_CODE, `manmade-waterbodies`) %>% distinct(), c("WATERSHED_GROUP_CODE"))
lookup %<>% left_join(glacier %>% select(WATERSHED_GROUP_CODE, `glaciers`) %>% distinct(), c("WATERSHED_GROUP_CODE"))

lookup_wsgroup <- lookup %>%
  modify_if(.p = ~ is.logical(.), .f = ~ replace_na(., FALSE))

lookup_wsgroup$`watershed-groups` <- TRUE
lookup_wsgroup %<>% left_join(wsgroup %>% select(WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME), "WATERSHED_GROUP_CODE")

use_data(lookup_gnis, lookup_wskey, lookup_wsgroup, internal = TRUE, overwrite = TRUE)
