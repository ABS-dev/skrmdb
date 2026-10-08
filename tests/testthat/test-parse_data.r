test_that(".gcd", {
  x <- sample(10:100, 1)
  expect_equal(.gcd(0, x), x)
  expect_equal(.gcd(x, 0), x)
  expect_equal(.gcd(x, x), x)
  expect_equal(.gcd(10, 15), 5)
  expect_equal(.gcd(15, 10), 5)
  expect_equal(.gcd(72, 54), 18)
  expect_equal(.gcd(18, 72), 18)
})

test_that(".lcm", {
  expect_equal(.lcm(90, 72), 360)
  expect_equal(.lcm(72, 90), 360)
  expect_equal(.lcm(c(90, 72)), 360)
  expect_equal(.lcm(c(72, 90)), 360)
  expect_equal(.lcm(2 * 3, 3 * 5, 5 * 7, 7 * 2), 2 * 3 * 5 * 7)
  expect_equal(.lcm(c(2 * 3, 3 * 5, 5 * 7, 7 * 2)), 2 * 3 * 5 * 7)
})

test_that(".parse_formula", {
  expect_error(.parse_formula())

  y <- n <- x <- 1:10
  res <- .parse_formula(y = y,
                        n = n,
                        x = x)
  expect_true(inherits(res, "CVB_formula"))
  expect_equal(res$data,
               data.table(
                 y = y,
                 n = n,
                 x = x))

  expect_error(.parse_formula(y = 1:10,
                              n = 1:10,
                              x = 1:11))
  expect_error(.parse_formula(y = 1:10,
                              n = 1:11,
                              x = 1:10))
  expect_error(.parse_formula(y = 1:11,
                              n = 1:10,
                              x = 1:10))
  y <- n <- x <- NULL
  fctn <- y + n ~ x
  y <- sample(1:100, 10)
  n <- sample(1:100, 10) + y
  x <- rep(1:5, each = 2)
  data <- data.table(
    y = y,
    n = n,
    x = x
  )
  res1 <- .parse_formula(data, fctn)
  res2 <- .parse_formula(fctn, data)
  expect_equal(res1$data, res2$data)

  data <- data.table(
    y = rep(y, times = 2),
    n = rep(n, times = 2),
    x = rep(x, times = 2)
  )
  expect_message(.parse_data(data, fctn, warn.me = TRUE), "duplicate dilutions")
})

test_that(".parse_data", {
  expect_true(TRUE)
})
