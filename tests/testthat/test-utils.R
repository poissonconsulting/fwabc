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

  wsg <- unique(lookup_wskey$WATERSHED_GROUP_CODE)[2]
  y <- c(lookup_wskey$WATERSHED_KEY[1], wsg)
  x <- wsgcode_to_wskey(y)
  z <- wsgcode_to_wskey(wsg)
  expect_true(length(x) - length(z) == 1)
  expect_true(all(is_wskey(x)))
  expect_length(x, 1464L)
  expect_identical(setdiff(x, z), lookup_wskey$WATERSHED_KEY[1])

  wsk <- x[1:2]
  wsg2 <- wskey_to_wsgcode(x[1:2])
  expect_identical(wsg2, c("PORI", "UPET"))

})
