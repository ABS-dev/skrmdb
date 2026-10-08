test_that("getED50", {
  expect_true(TRUE)
})

test_that("getED", {
  expect_true(TRUE)
})

test_that("getVar", {
  expect_true(TRUE)
})

test_that("getvar", {
  expect_true(TRUE)
})

test_that("getData", {
  expect_true(TRUE)
})

test_that("getdata", {
  expect_true(TRUE)
})

test_that("getResults", {
  expect_true(TRUE)
})

test_that("print.skrmdb", {
  expect_true(TRUE)
})

test_that("print.skrmdb::pad", {
  # extract pad from print.skrmdb
  pad <- find_sub_function(print.skrmdb, "pad")
  expect_false(is.null(pad))
  expect_true(is.function(pad))

  expect_true(TRUE)
})

test_that("new_skrmdb", {
  expect_true(TRUE)
})

test_that("new_skrmdb::.order_cols", {
  # extract .order_cols from new_skrmdb
  .order_cols <- find_sub_function(new_skrmdb, ".order_cols")
  expect_false(is.null(.order_cols))
  expect_true(is.function(.order_cols))
  expect_true(TRUE)
})

test_that("new_skrmdb::.interpret_msg", {
  # extract .interpret_msg from new_skrmdb
  .interpret_msg <- find_sub_function(new_skrmdb, ".interpret_msg")
  expect_false(is.null(.interpret_msg))
  expect_true(is.function(.interpret_msg))
  expect_true(TRUE)
})
