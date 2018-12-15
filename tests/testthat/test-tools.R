context("tools")

test_that("tools work", {

  ### search funs
  expect_identical(fwa_search_gnis("kaslo"), "Kaslo River")
  expect_identical(character(0), fwa_search_gnis("kaslo", ignore_case = FALSE))

  expect_identical(fwa_search_wsgroup("porcher"), "Porcher Island")
  expect_identical(character(0), fwa_search_wsgroup("porcher", code = TRUE))
  expect_identical(character(0), fwa_search_wsgroup("kootenay", coast = TRUE))
  expect_length(fwa_search_wsgroup("kootenay"), 2L)

  ### rkm
  x <- fwa_rkm(stream = "Chown Brook", label_name = "rkmtest", distance = 100, blkey_name = "blktest", sfc_name = "geom")
})
