library(poispkgs)

dsn_bc <- "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb/"
dsn_wsl <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHED_BOUNDARIES_SP.gdb/"
dsn_wsp <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb/"
dsn_lb <- "~/Poisson/Data/spatial/fwa/gdb/FWA_LINEAR_BOUNDARIES_SP.gdb/"

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

route <- st_read(dsn_bc, layer = "FWA_ROUTES_SP", query = "select BLUE_LINE_KEY, FWA_WATERSHED_CODE, WATERSHED_KEY from FWA_ROUTES_SP")

fwa_stream_lookup <- route %>%
  st_set_geometry(NULL) %>%
  select(BlueLineKey = BLUE_LINE_KEY,
         WatershedCode = FWA_WATERSHED_CODE,
         WatershedKey = WATERSHED_KEY)

ws <- st_read(dsn_bc, layer = "FWA_NAMED_WATERSHEDS_POLY") %>%
  select(GNIS_NAME, BLUE_LINE_KEY, STREAM_ORDER)

fwa_gnis <- ws %>%
  st_set_geometry(NULL) %>%
  select(BlueLineKey = BLUE_LINE_KEY,
         GnisName = GNIS_NAME)
fwa_gnis$GnisName %<>% as.character

# remove Colley Creek BlueLineKey that isn't present in the route table
fwa_gnis %<>% filter(!(BlueLineKey %in% anti_join(fwa_gnis, fwa_stream_lookup, "BlueLineKey")$BlueLineKey))
check_join(fwa_gnis, fwa_stream_lookup, "BlueLineKey")
fwa_stream_lookup <- left_join(fwa_stream_lookup, fwa_gnis, "BlueLineKey")

usethis::use_data(fwa_stream_lookup, overwrite = TRUE)

fwa_gnis_lookup <- fwa_gnis
usethis::use_data(fwa_gnis_lookup, overwrite = TRUE)

wsgroup <- st_read(dsn_bc, layer = "FWA_WATERSHED_GROUPS_POLY")

fwa_watershed_group <- wsgroup %>%
  st_set_geometry(NULL) %>%
  select(WatershedGroupCode =  WATERSHED_GROUP_CODE,
         WatershedGroupName = WATERSHED_GROUP_NAME)
fwa_watershed_group$WatershedGroupCode %<>% as.character
fwa_watershed_group$WatershedGroupName %<>% as.character

fwa_wsgroup_lookup <- fwa_watershed_group
usethis::use_data(fwa_wsgroup_lookup, overwrite = TRUE)

fwa_coastline_lookup <- st_read(dsn_bc, layer = "FWA_COASTLINES_SP",
                             query = "select * from FWA_COASTLINES_SP") %>%
  st_set_geometry(NULL) %>%
  select(BlueLineKey = BLUE_LINE_KEY,
         WatershedCode = FWA_WATERSHED_CODE,
         WatershedKey = WATERSHED_KEY)

usethis::use_data(fwa_coastline_lookup, overwrite = TRUE)

