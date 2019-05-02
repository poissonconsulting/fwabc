context("fwa read")

test_that("read fwa data", {

  ###### ------ fwa_read
  # works with WATERSHED_KEY
  streams <- c(360709847, 360843586)
  x <- fwa_read(streams, layer = "stream-network")
  expect_identical(nrow(x), 43L)
  expect_is(x, "sf")
  expect_equal(st_crs(x), 3005L)

  y <- bcdata::bcdc_query_geodata("freshwater-atlas-stream-network") %>%
    bcdata::filter(WATERSHED_KEY %in% c(360709847, 360843586)) %>%
    bcdata::collect()
  testthat::expect_equal(x %>% st_set_geometry(NULL), y %>% st_set_geometry(NULL))

  # works with WATERSHED_GROUP_CODE
  streams <- c("PORI")
  x <- fwa_read(streams, layer = "stream-network", crs = 4326)
  expect_equal(st_crs(x), 4326L)
  expect_identical(nrow(x), 5203L)
  expect_is(x, "sf")

  y <- bcdata::bcdc_query_geodata("freshwater-atlas-stream-network") %>%
    bcdata::filter(WATERSHED_GROUP_CODE %in% "PORI") %>%
    bcdata::collect()

  testthat::expect_equal(x %>% st_set_geometry(NULL), y %>% st_set_geometry(NULL))

  x <- c(360843586)

  # test convenience functions work
  # streams
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`stream-network`][1]
  expect_identical(fwa_read_streams(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "stream-network")$WATERSHED_KEY)
  # coastlines
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`coastlines`][1]
  expect_identical(fwa_read_coastlines(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "coastlines")$WATERSHED_KEY)
  # watersheds
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`watersheds`][1]
  expect_identical(fwa_read_watersheds(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "watersheds")$WATERSHED_KEY)
  # obstructions
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`obstructions`][1]
  expect_identical(fwa_read_obstructions(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "obstructions")$WATERSHED_KEY)
  # linear-boundaries
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`linear-boundaries`][1]
  expect_identical(fwa_read_linear_boundaries(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "linear-boundaries")$WATERSHED_KEY)
  # lakes
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`lakes`][1]
  expect_identical(fwa_read_lakes(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "lakes")$WATERSHED_KEY)
  # rivers
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`rivers`][1]
  expect_identical(fwa_read_rivers(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "rivers")$WATERSHED_KEY)
  # wetlands
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`wetlands`][1]
  expect_identical(fwa_read_wetlands(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "wetlands")$WATERSHED_KEY)
  # manmade-waterbodies
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`manmade-waterbodies`][1]
  expect_identical(fwa_read_manmade_waterbodies(x)$WATERSHED_KEY,
                   fwa_read(x, layer = "manmade-waterbodies")$WATERSHED_KEY)
  # watershed_groups
  x <- "PORI"
  expect_identical(fwa_read_watershed_groups(x)$WATERSHED_GROUP_CODE,
                   fwa_read(x, layer = "watershed-groups")$WATERSHED_GROUP_CODE)
  # glaciers
  x <- lookup_wsgroup$WATERSHED_GROUP_CODE[lookup_wsgroup$glaciers][1]
  expect_equal(fwa_read_glaciers(x)$WATERBODY_POLY_ID,
                   fwa_read(x, layer = "glaciers")$WATERBODY_POLY_ID)

  ###### ------ search functions
  x <- fwa_search_gnis("sangan|hiellen")
  y <- fwa_search_gnis("sangan|hiellen", ignore.case = FALSE)
  z <- fwa_search_gnis("sangan", layer = "lakes")





})
