context("fwa read")

test_that("read fwa data", {

  ### translators
  x <- c("Kaslo River", "Keen Creek", 356567321L)
  blk <- gnis_to_blk(x)
  expect_identical(blk, as.character(c(356567321L, 356562809L, 356567321L)))

  ### read streams (routes)
  dsn <- system.file(package = "poisspatial", "fwa/FWA_ROUTES_SP.gpkg")
  # provide gnis
  x <- ps_fwa_stream(dsn, stream = "Kaslo River")
  expect_true(nrow(x) == 1L)
  expect_is(x, "sf")
  expect_true(x$BLUE_LINE_KEY  == 356567321L)
  # provide blk
  x <- ps_fwa_stream(dsn, stream = 356567322L)
  expect_true(nrow(x) == 1L)
  expect_is(x, "sf")
  expect_true(x$BLUE_LINE_KEY == 356567322L)
  # combination
  x <- ps_fwa_stream(dsn, stream = c(356567322L, 356567323L, "Kaslo River"))
  expect_true(nrow(x) == 3L)
  expect_is(x, "sf")
  expect_true(all(x$BLUE_LINE_KEY %in% c(356567321L, 356567322L, 356567323L)))

  ### read coastline
  dsn <- system.file(package = "poisspatial", "fwa/FWA_COASTLINES_SP.gpkg")
  x <- ps_fwa_coastline(dsn = dsn, watershed_group = c("PORI", "Porcher Island"))
  expect_is(x, "sf")
  expect_true(nrow(x) == 2L)
  expect_true(all(x$WATERSHED_GROUP_CODE == "PORI"))
  x <- ps_fwa_coastline(dsn = dsn, blue_line_key = 380891035L)
  expect_is(x, "sf")
  expect_true(nrow(x) == 2L)
  expect_true(all(x$BLUE_LINE_KEY == 380891035L))

  expect_error(ps_fwa_coastline(dsn = dsn, watershed_group = 380891035L), "Error: 380891035 is not a valid WatershedGroupCode or WatershedGroupName")

  ws


  ### read watersheds


})
