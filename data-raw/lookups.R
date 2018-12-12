library(poispkgs)

dsn_bc <- "~/Poisson/Data/spatial/fwa/gdb/FWA_BC.gdb/"
dsn_wsl <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHED_BOUNDARIES_SP.gdb/"
dsn_wsp <- "~/Poisson/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb/"
dsn_lb <- "~/Poisson/Data/spatial/fwa/gdb/FWA_LINEAR_BOUNDARIES_SP.gdb/"

fwa_meta <- list(fwa_bc = st_layers(dsn_bc),
                 fwa_watershed_line = st_layers(dsn_wsl),
                 fwa_watershed_polygon = st_layers(dsn_wsp),
                 fwa_linear_boundaries = st_layers(dsn_lb))

fwa_layers <- lapply(fwa_meta, function(x) x$name[!is.na(x$geomtype)])

to_shortcut <- function(x){
  ex <- grep("_max|_fwa|50K|CODES", x, value = T)
  x <- setdiff(x, ex) %>%
    tolower %>%
    gsub("fwa_|_poly|_sp", "", .)
  x
}

fwa_layers$fwa_bc <- tibble(layer = fwa_layers$fwa_bc, shortcut = to_shortcut(fwa_layers$fwa_bc))
fwa_layers[2:4] <- lapply(fwa_layers[2:4], function(x) tibble(layer = x, shortcut = wscode_to_name(x)))

usethis::use_data(fwa_meta, overwrite = TRUE)
usethis::use_data(fwa_layers, overwrite = TRUE)

blk <- st_read(dsn_bc, layer = "FWA_ROUTES_SP", query = "select BLUE_LINE_KEY, FWA_WATERSHED_CODE from FWA_ROUTES_SP")
ws <- st_read(dsn_bc, layer = "FWA_NAMED_WATERSHEDS_POLY") %>%
  select(GNIS_NAME, BLUE_LINE_KEY, STREAM_ORDER)
wsgroup <- st_read(dsn_bc, layer = "FWA_WATERSHED_GROUPS_POLY")

fwa_blk_stream <- blk %>%
  st_set_geometry(NULL) %>%
  select(BlueLineKey = BLUE_LINE_KEY)

usethis::use_data(fwa_blk_stream, overwrite = TRUE)

fwa_gnis <- ws %>%
  st_set_geometry(NULL) %>%
  select(BlueLineKey = BLUE_LINE_KEY,
         GnisName = GNIS_NAME)
fwa_gnis$GnisName %<>% as.character

# remove Colley Creek BlueLineKey that isn't present in the route table
fwa_gnis %<>% filter(!(BlueLineKey %in% anti_join(gnis, blkey, "BlueLineKey")$BlueLineKey))

usethis::use_data(fwa_gnis, overwrite = TRUE)

fwa_watershed_group <- wsgroup %>%
  st_set_geometry(NULL) %>%
  select(WatershedGroupCode =  WATERSHED_GROUP_CODE,
         WatershedGroupName = WATERSHED_GROUP_NAME)
fwa_watershed_group$WatershedGroupCode %<>% as.character
fwa_watershed_group$WatershedGroupName %<>% as.character

usethis::use_data(fwa_watershed_group, overwrite = TRUE)

fwa_blk_coastline <- st_read(dsn_bc, layer = "FWA_COASTLINES_SP",
                             query = "select BLUE_LINE_KEY from FWA_COASTLINES_SP")  %>%
  st_set_geometry(NULL) %>%
  select(BlueLineKey = BLUE_LINE_KEY)

usethis::use_data(fwa_blk_coastline, overwrite = TRUE)

