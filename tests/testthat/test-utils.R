context("utils")

test_that("utils work", {

  ### translators
  x <- is_wskey(c("Kaslo River", 356567321L))
  expect_identical(x, c(FALSE, TRUE))
  expect_false(is_wskey(356567321L, layer = "coastlines"))

  y <- lookup_wskey$FWA_WATERSHED_CODE[1]
  x <- is_wscode(c(y, 356567321L))
  expect_identical(x, c(TRUE, FALSE))
  expect_false(is_wskey(y, layer = "coastlines"))

  x <- is_wsgcode(c("GRAI", 356567321L))
  expect_identical(x, c(TRUE, FALSE))
  expect_false(is_wsgcode("UPET", layer = "coastlines"))

  x <- is_wsgname(c("Graham Island", 356567321L))
  expect_identical(x, c(TRUE, FALSE))
  expect_false(is_wsgname("Upper Shuswap", layer = "coastlines"))

  x <- is_gnis(c("Sangan River", 356567321L))
  expect_identical(x, c(TRUE, FALSE))
  expect_false(is_wskey("Porcher Creek", layer = "coastlines"))

  ### tributaries
  x <- tribs(lookup_wskey$FWA_WATERSHED_CODE[1], n = 1L)
  expect_true(all(is_wskey(x)))
  expect_length(x, 8L)

  x <- tribs(lookup_wskey$FWA_WATERSHED_CODE[1], n = 2L)
  expect_true(all(is_wskey(x)))
  expect_length(x, 10L)

  ### converters
  y <- c(lookup_wskey$WATERSHED_KEY[1], lookup_wskey$FWA_WATERSHED_CODE[2])
  x <- wskey_to_wscode(y)
  expect_length(x, 2L)
  expect_identical(x[2], lookup_wskey$FWA_WATERSHED_CODE[1])
  expect_true(all(is_wscode(x)))

  ### filters
  wskey <- c(360709847, 360843586)
  x <- filter_wskey(wskey, layer = "stream-network", crs = 3005)
  expect_identical(nrow(x), 43L)
  expect_is(x, "sf")
  expect_equal(sf::st_crs(x)$epsg, 3005)

  wsgcode <- c("VICT", "LKEL")
  x <- filter_wsgcode(wsgcode, layer = "stream-network", crs = 4326)
  expect_equal(sf::st_crs(x)$epsg, 4326L)
  expect_identical(nrow(x), 4526L)
  expect_is(x, "sf")

  x <- filter_both(wskey = 360709847, wsgcode = "VICT",
                   layer = "stream-network", crs = 4326)
  expect_identical(nrow(x), 2161L)
  expect_is(x, "sf")

})
