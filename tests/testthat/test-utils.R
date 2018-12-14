context("utils")

test_that("utils work", {

  ### translators
  x <- stream_to_blk(c("Kaslo River", 356567321L))
  expect_length(x, 2L)
  expect_is(x, "character")
  expect_true(all(x == 356567321L))

  x <- stream_to_wscode(c("Kaslo River", "300-625474-095953-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000"))
  expect_length(x, 2L)
  expect_is(x, "character")
  expect_true(all(x == "300-625474-095953-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000"))

  x <- wsgname_to_wsgcode(c("Porcher Island", "PORI", 356567321L))
  expect_length(x, 3L)
  expect_is(x, "character")
  expect_identical(x,  c("PORI", "PORI", "356567321"))

  ### tributaries
  x <- tribs_stream(stream_to_wscode("Chown Brook"))
  expect_is(x, "integer")
  expect_true(all(is_blk_stream(x)))
  expect_length(x, 8L)

  x <- tribs_wshed(stream_to_wscode("Chown Brook"))
  expect_is(x, "character")
  expect_true(all(is_ws_code_stream(x)))
  expect_length(x, 8L)

  ### search funs
  expect_identical(fwa_search_gnis("kaslo"), "Kaslo River")
  expect_identical(character(0), fwa_search_gnis("kaslo", ignore_case = FALSE))

  expect_identical(fwa_search_wsgroup("porcher"), "Porcher Island")
  expect_identical(character(0), fwa_search_wsgroup("porcher", code = TRUE))
  expect_identical(character(0), fwa_search_wsgroup("kootenay", coast = TRUE))
  expect_length(fwa_search_wsgroup("kootenay"), 2L)


})
