context("fwa read")

test_that("read fwa data", {

  ###### ------ fwa_read
  # works with WATERSHED_GROUP_CODE
  ws <- c("VICT", 360709847)
  x <- fwa_read(ws[1], layer = "stream-network")
  y <- fwa_read(ws[2], layer = "stream-network")
  z <- fwa_read(ws, layer = "stream-network")

  expect_equal(nrow(rbind(x, y)), nrow(z))
  expect_equal(names(rbind(x, y)), names(z))

  # test convenience functions work
  # streams
  x <- lookup_wskey$WATERSHED_KEY[lookup_wskey$`stream-network`][1]
  expect_identical(fwa_read_stream_network(x)$WATERSHED_KEY,
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
  # search gnis
  expect_identical(fwa_search_gnis("sangan|hiellen"),
                   c("Hiellen River", "Sangan River"))
  expect_identical(fwa_search_gnis("porcher", layer = "lakes"),
                   "Porcher Creek")
  # test that args passed to grepl
  expect_length(fwa_search_gnis("sangan|hiellen", ignore_case = FALSE), 0)
  expect_length(fwa_search_gnis("Sangan|Hiellen",
                                fixed = TRUE, ignore_case = FALSE), 0)
  expect_identical(fwa_search_gnis("sangan|hiellen", layer = "lakes"),
                   "Hiellen River")

  # search wsgroups
  expect_identical(fwa_search_watershed_group("porcher|graham"),
                   c("Porcher Island", "Graham Island"))
  expect_identical(fwa_search_watershed_group("porcher", layer = "lakes"),
                   "Porcher Island")

  ###### ------ pull functions
  gnis <- c("Sangan River", "Hiellen River")
  wskey <- lookup_gnis$WATERSHED_KEY[lookup_gnis$GNIS_NAME %in% gnis]
  expect_error(fwa_pull_watershed_key("sangan"))
  expect_identical(fwa_pull_watershed_key(gnis), wskey)

  wsg <- c("Porcher Island", "Graham Island")
  wscode <- lookup_wsgroup$WATERSHED_GROUP_CODE[lookup_wsgroup$WATERSHED_GROUP_NAME %in% wsg]
  expect_error(fwa_pull_watershed_group_code("PORI"))
  expect_identical(fwa_pull_watershed_group_code(wsg), wscode)

  x <- fwa_pull_tributaries(wskey, 1)
  y <- fwa_pull_tributaries(wskey, 2)
  z <- fwa_pull_tributaries(wskey[1], 1)

  expect_true(length(y) > length(x))
  expect_true(all(is_wskey(x)))
  expect_length(z, 35)
  expect_error(fwa_pull_tributaries("Sangan River"))

  # check that can process large vectors
  pori <- bcdata::bcdc_query_geodata("freshwater-atlas-stream-network") %>%
    bcdata::filter(WATERSHED_GROUP_CODE %in% "PORI") %>%
    bcdata::collect()

  # fails
  expect_is(bcdata::bcdc_query_geodata("freshwater-atlas-stream-network") %>%
    bcdata::filter(WATERSHED_KEY %in% pori$WATERSHED_KEY) %>%
    bcdata::collect(), "sf")
})
