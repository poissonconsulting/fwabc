library(dplyr)
library(sf)
library(purrr)
library(magrittr)
library(checkr)

dsn_bc <- "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb/"
dsn_wsl <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHED_BOUNDARIES_SP.gdb/"
dsn_wsp <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb/"
dsn_lb <- "~/Poisson/Data/spatial/fwa/gdb/FWA_LINEAR_BOUNDARIES_SP.gdb/"

###### ------ layers and metadata
fwa_metadata <- list(FWA_BC = st_layers(dsn_bc),
                     FWA_WATERSHED_BOUNDARIES_SP = st_layers(dsn_wsl),
                     FWA_WATERSHEDS_POLY = st_layers(dsn_wsp),
                     FWA_LINEAR_BOUNDARIES_SP = st_layers(dsn_lb))

fwa_layers <- lapply(fwa_metadata, function(x) x$name[!is.na(x$geomtype)])
fwa_layers <- map_dfr(names(fwa_metadata), function(x){
  data <- fwa_metadata[x]
  tibble(db = x, layer = data[[1]]$name)
})

usethis::use_data(fwa_metadata, overwrite = TRUE)
usethis::use_data(fwa_layers, overwrite = TRUE)

###### ------ streams
route <- st_read(dsn_bc, layer = "FWA_ROUTES_SP", query = "select BLUE_LINE_KEY, FWA_WATERSHED_CODE, WATERSHED_KEY from FWA_ROUTES_SP")

route <- route %>%
  st_set_geometry(NULL) %>%
  select(BLUE_LINE_KEY, FWA_WATERSHED_CODE)
route$FWA_WATERSHED_CODE %<>% as.character()

check_key(route, "BLUE_LINE_KEY")

###### ------ gnis
gnis <- st_read(dsn_bc, layer = "FWA_NAMED_WATERSHEDS_POLY") %>%
  select(GNIS_NAME, BLUE_LINE_KEY)

gnis <- gnis %>%
  st_set_geometry(NULL)

gnis$GNIS_NAME %<>% as.character()

# remove Colley Creek BlueLineKey that isn't present in the route table
gnis %<>% filter(!(BLUE_LINE_KEY %in% anti_join(gnis, route, "BLUE_LINE_KEY")$BLUE_LINE_KEY))
check_join(gnis, route, "BLUE_LINE_KEY")

fwa_lookup_stream <- route %>%
  left_join(gnis, "BLUE_LINE_KEY")

# note there are ~ 10 cases where the same BlueLineKey has multiple GnisNames!! BLUE_LINE_KEY is not the primary key
check_key(fwa_lookup_stream, c("BLUE_LINE_KEY", "GNIS_NAME"))
usethis::use_data(fwa_lookup_stream, overwrite = TRUE)

###### ------ watershed groups
wsgroup <- st_read(dsn_bc, layer = "FWA_WATERSHED_GROUPS_POLY")

wsgroup <- wsgroup %>%
  st_set_geometry(NULL) %>%
  select(WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME)
wsgroup$WATERSHED_GROUP_CODE %<>% as.character()
wsgroup$WATERSHED_GROUP_NAME %<>% as.character()

fwa_lookup_watershedgroup <- wsgroup
usethis::use_data(fwa_lookup_watershedgroup, overwrite = TRUE)

###### ------ coastline
coastline <- st_read(dsn_bc, layer = "FWA_COASTLINES_SP",
                     query = "select BLUE_LINE_KEY, FWA_WATERSHED_CODE, WATERSHED_GROUP_CODE from FWA_COASTLINES_SP") %>%
  st_set_geometry(NULL) %>%
  distinct()

coastline$WATERSHED_GROUP_CODE %<>% as.character()

check_join(coastline, wsgroup, "WATERSHED_GROUP_CODE")
fwa_lookup_coastline <- coastline
usethis::use_data(fwa_lookup_coastline, overwrite = TRUE)

###### ------ watershed polygons
layers <- st_layers(dsn_wsp)$name

fwa_lookup_watershed <- map_dfr(layers, ~ st_read(dsn = dsn_wsp, layer = ., query = paste("select FWA_WATERSHED_CODE, WATERSHED_GROUP_CODE from", .)) %>%
                                          st_set_geometry(NULL)) %>%
  distinct()

usethis::use_data(fwa_lookup_watershed, overwrite = TRUE)

