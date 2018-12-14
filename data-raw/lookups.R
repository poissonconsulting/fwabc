library(poispkgs)

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
  select(BlueLineKey = BLUE_LINE_KEY,
         WatershedCode = FWA_WATERSHED_CODE)
route$WatershedCode %<>% as.character

check_key(route, "BlueLineKey")

fwa_lookup_stream_blkey <- route
usethis::use_data(fwa_lookup_stream_blkey, overwrite = TRUE)

###### ------ gnis
gnis <- st_read(dsn_bc, layer = "FWA_NAMED_WATERSHEDS_POLY") %>%
  select(GNIS_NAME, BLUE_LINE_KEY, STREAM_ORDER)

gnis <- gnis %>%
  st_set_geometry(NULL) %>%
  select(BlueLineKey = BLUE_LINE_KEY,
         GnisName = GNIS_NAME)
gnis$GnisName %<>% as.character

# remove Colley Creek BlueLineKey that isn't present in the route table
gnis %<>% filter(!(BlueLineKey %in% anti_join(gnis, route, "BlueLineKey")$BlueLineKey))
check_join(gnis, route, "BlueLineKey")

# get WatershedCode
# note there are ~ 10 cases where the same BlueLineKey has multiple GnisNames!!
check_join(gnis, route, "BlueLineKey")
fwa_lookup_stream_gnis <- left_join(gnis, route, "BlueLineKey")
usethis::use_data(fwa_lookup_stream_gnis, overwrite = TRUE)

###### ------ watershed groups
wsgroup <- st_read(dsn_bc, layer = "FWA_WATERSHED_GROUPS_POLY")

wsgroup <- wsgroup %>%
  st_set_geometry(NULL) %>%
  select(WatershedGroupCode =  WATERSHED_GROUP_CODE,
         WatershedGroupName = WATERSHED_GROUP_NAME)
wsgroup$WatershedGroupCode %<>% as.character
wsgroup$WatershedGroupName %<>% as.character

fwa_lookup_wsgroup <- wsgroup
usethis::use_data(fwa_lookup_wsgroup, overwrite = TRUE)

###### ------ coastline
coastline <- st_read(dsn_bc, layer = "FWA_COASTLINES_SP",
                     query = "select * from FWA_COASTLINES_SP")

cblk <- coastline %>%
  st_set_geometry(NULL) %>%
  select(BlueLineKey = BLUE_LINE_KEY,
         WatershedCode = FWA_WATERSHED_CODE)

cwsg <- coastline %>%
  st_set_geometry(NULL) %>%
  select(WatershedGroupCode = WATERSHED_GROUP_CODE) %>%
  distinct

cwsg$WatershedGroupCode %<>% as.character
cblk$WatershedCode %<>% as.character

check_join(cwsg, wsgroup, "WatershedGroupCode")
cwsg <- cwsg %>%
  left_join(wsgroup, "WatershedGroupCode")

fwa_lookup_coast_blkey <- cblk
fwa_lookup_coast_wsgroup <- cwsg

usethis::use_data(fwa_lookup_coast_blkey, overwrite = TRUE)
usethis::use_data(fwa_lookup_coast_wsgroup, overwrite = TRUE)

###### ------ watershed polygons
layers <- st_layers(dsn_wsp)$name

fwa_lookup_watershed <- map_dfr(layers, ~ st_read(dsn = dsn_wsp, layer = ., query = paste("select FWA_WATERSHED_CODE, WATERSHED_GROUP_CODE from", .)) %>%
                                          st_set_geometry(NULL))

fwa_lookup_watershed %<>% distinct
fwa_lookup_watershed %<>% rename(WatershedCode = FWA_WATERSHED_CODE,
                                         WatershedGroupCode = WATERSHED_GROUP_CODE)

usethis::use_data(fwa_lookup_watershed, overwrite = TRUE)


