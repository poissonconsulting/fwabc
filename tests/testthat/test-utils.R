context("utils")

test_that("utils work", {

  ### translators
  x <- is_wskey(c("Kaslo River", 356567321L))
  expect_identical(x, c(FALSE, TRUE))
  expect_false(is_wskey(356567321L, layer = "coastlines"))
  expect_true(is_wskey(356567321L, layer = "stream-network"))

  y <- lookup_wskey$FWA_WATERSHED_CODE[1]
  x <- is_wscode(c(y, 356567321L))
  expect_identical(x, c(TRUE, FALSE))
  expect_false(is_wskey(y, layer = "coastlines"))

  x <- is_wsgcode(c("GRAI", 356567321L))
  expect_identical(x, c(TRUE, FALSE))
  expect_false(is_wsgcode("UPET", layer = "coastlines"))
  expect_true(is_wsgcode("UPET", layer = "watershed-groups"))

  x <- is_wsgname(c("Graham Island", 356567321L))
  expect_identical(x, c(TRUE, FALSE))
  expect_false(is_wsgname("Upper Shuswap", layer = "coastlines"))
  expect_true(is_wsgname("Upper Shuswap", layer = "watershed-groups"))

  x <- is_gnis(c("Sangan River", 356567321L))
  expect_identical(x, c(TRUE, FALSE))
  expect_false(is_wskey("Porcher Creek", layer = "coastlines"))
  expect_false(is_gnis("Porcher Creek", layer = "glaciers"))
  expect_true(is_gnis("Porcher Creek", layer = "stream-network"))

  ### converters
  y <- c(lookup_wskey$WATERSHED_KEY[1], lookup_wskey$FWA_WATERSHED_CODE[2])
  x <- wskey_to_wscode(y, layer = "stream-network")
  expect_identical(x, "915-749008-900977")
  expect_length(wskey_to_wscode(y, layer = "manmade-waterbodies"), 0L)

  y <- c(lookup_gnis$GNIS_NAME[1], lookup_gnis$FWA_WATERSHED_CODE[2])
  x <- gnis_to_wscode(y, layer = "stream-network")
  expect_identical(x, "915-724877-268923")
  expect_length(gnis_to_wscode(y, layer = "manmade-waterbodies"), 0L)

  y <- "Graham Island"
  x <- wsgname_to_wsgcode(y)
  expect_identical(x, "GRAI")
  expect_identical(wsgcode_to_wsgname(x), y)

})
