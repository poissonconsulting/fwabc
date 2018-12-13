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
  expect_true(x == "300-625474-095953-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000-000000")

  x <- wsgname_to_wsgcode("Porcher Island")
  expect_length(x, 1L)
  expect_is(x, "character")
  expect_true(x == "PORI")

  ### tributaries
  x <- tribs(stream_to_wscode("Chown Brook"))
  expect_is(x, "integer")
  expect_length(x, 8L)
})
