library(poispkgs)

dsn_wsl <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHED_BOUNDARIES_SP.gdb/"
dsn_wsp <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb/"

layers <- st_layers(dsn_wsp)$name

fwa_lookup_watershed_polygon <- map_dfr(layers, ~ st_read(dsn = dsn_wsp, layer = ., query = paste("select FWA_WATERSHED_CODE, WATERSHED_GROUP_CODE from", .)) %>%
  st_set_geometry(NULL))

fwa_wscode_lookup <- fwa_wscode_lookup %>% distinct
check_join(fwa_wscode_lookup, fwa_stream_lookup, c("FWA_WATERSHED_CODE" = "WatershedCode"))


fwa_wscode_lookup2 <- map_dfr(layers[1], ~ st_read(dsn = dsn_wsl, layer = ., query = paste("select * from", .)) %>%
                               st_set_geometry(NULL))

