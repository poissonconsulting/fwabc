context("fwa read")

test_that("read fwa data", {

  ### read streams
  dsn <- system.file("extdata", "stream.gpkg", package = "fwabc", mustWork = TRUE)
  streams <- c("Chown Brook", 360456318L, "940-971473-016923-822158-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000")
  x <- fwa_stream(streams, dsn = dsn)
  expect_identical(nrow(x), 3L)
  expect_is(x, "sf")
  expect_identical(x$BLUE_LINE_KEY, c(360456318L, 360545767L, 360640895L))

  x <- fwa_stream("Chown Brook", tributaries = TRUE, dsn = dsn)
  expect_identical(nrow(x), 8L)
  expect_is(x, "sf")

  x2 <- fwa_stream(dsn = dsn, ask = FALSE)
  expect_identical(x, x2)

  ### read coastline
  dsn <- system.file("extdata", "coastline.gpkg", package = "fwabc", mustWork = TRUE)
  x <- fwa_coastline(c(380891035L, "915-764826-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000"), dsn = dsn)
  expect_is(x, "sf")
  expect_true(nrow(x) == 3L)
  expect_identical(unique(x$WATERSHED_GROUP_CODE) %>% as.character, c("PORI"))

  x <- fwa_coastline(c("SEYM", "Porcher Island"), dsn = dsn)
  expect_is(x, "sf")
  expect_identical(nrow(x), 12L)
  expect_identical(unique(x$WATERSHED_GROUP_CODE) %>% as.character, c("PORI", "SEYM"))

  x2 <- fwa_coastline(dsn = dsn)
  expect_identical(x, x2)

  ### read watershed groups
  dsn <- system.file("extdata", "wsgroup.gpkg", package = "fwabc", mustWork = TRUE)
  x <- fwa_watershed_group(c("NBNK", "North Banks Island"), dsn = dsn)
  expect_is(x, "sf")
  expect_identical(nrow(x), 1L)
  expect_identical(x$WATERSHED_GROUP_CODE %>% as.character, "NBNK")
  expect_identical(ncol(x), 8L)

  x2 <- fwa_watershed_group(dsn = dsn)
  expect_identical(x, x2)

  ### read watersheds
  dsn <- system.file("extdata", "wshed.gpkg", package = "fwabc", mustWork = TRUE)
  wsheds <- c("Kliki Damen Creek", "Hiellen River")
  x <- fwa_watershed(wsheds, watershed_group = "GRAI", dsn = dsn)
  expect_is(x, "sf")
  expect_identical(nrow(x), 4L)
  expect_identical(ncol(x), 37L)
  expect_true(all(x$WATERSHED_GROUP_CODE == "GRAI"))

  x <- fwa_watershed(wsheds, watershed_group = "GRAI", tributaries = TRUE, dsn = dsn)
  expect_identical(nrow(x), 10L)
  expect_identical(ncol(x), 37L)
  expect_true(all(x$WATERSHED_GROUP_CODE == "GRAI"))

  x2 <- fwa_watershed(dsn = dsn)
  expect_identical(x2, x)

})
