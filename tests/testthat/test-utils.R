context("utils")

test_that("utils work", {

  ### translators
  x <- stream_to_blk("Kaslo River")
  expect_length(x, 1L)
  expect_is(x, "integer")
  expect_true(x == 356567321L)

  x <- stream_to_wscode("Kaslo River")
  expect_length(x, 1L)
  expect_is(x, "character")
  expect_true(x == 356567321L)

  x <- wsgname_to_wsgcode("PORI")
  expect_length(x, 1L)
  expect_is(x, "character")
  expect_true(x == "Porcher Island")

  ### tributaries
  x <- tribs(stream_to_wscode("Chown River"))
  expect_is(x, "integer")
  expect_length(x, 8L)
})
