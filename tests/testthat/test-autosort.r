test_that("autosort", {
  alive <- c(10, 10, 8, 7, 3, 1, 0)
  n     <- rep(10, length(alive))
  x     <- seq_along(alive)
  dead  <- n - alive
  res1  <- SpearKarb(dead + n ~ x)
  res2  <- SpearKarb(alive + n ~ x)
  res3  <- SpearKarb(alive + n ~ x, autosort = FALSE)
  expect_equal(res1$ed, res2$ed)
  expect_false(res1$ed == res3$ed)
})
