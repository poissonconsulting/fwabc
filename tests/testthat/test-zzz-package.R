context("fwa read")

test_that("read fwa data", {

  ### read streams
  dsn <- system.file("extdata", "stream.gpkg", package = "fwabc", mustWork = TRUE)
  streams <- c("Chown Brook", 360456318L, "940-971473-016923-822158-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000")
  x <- fwa_stream(stream = streams, dsn = dsn)
  expect_true(nrow(x) == 3L)
  expect_is(x, "sf")
  expect_true(all(x$BLUE_LINE_KEY  %in% c(360456318L, 360545767L, 360640895L)))

  x <- fwa_stream(stream = "Chown Brook", tributaries = TRUE, dsn = dsn)
  expect_true(nrow(x) == 8L)
  expect_is(x, "sf")

  ### read coastline
  dsn <- system.file("extdata", "coastline.gpkg", package = "fwabc", mustWork = TRUE)
  x <- fwa_coastline(coastline = c(380891035L, "915-764826-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000"), dsn = dsn)
  expect_is(x, "sf")
  expect_true(nrow(x) == 3L)
  expect_identical(unique(x$WATERSHED_GROUP_CODE) %>% as.character, c("PORI"))

  x <- fwa_coastline(coastline = c("SEYM", "Porcher Island"), dsn = dsn)
  expect_is(x, "sf")
  expect_true(nrow(x) == 12L)
  expect_identical(unique(x$WATERSHED_GROUP_CODE) %>% as.character, c("PORI", "SEYM"))

  ### read watershed groups

  ### read watersheds
  x <- fwa_watershed(watershed = c())

})
