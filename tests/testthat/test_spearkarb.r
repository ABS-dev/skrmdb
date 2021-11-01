context("SpearKarb")

#All Good
test_that("examples work", {
  y = c(0, 1, 2, 3, 4)
  n = c(4, 4, 4, 4, 4)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 3,
                var  = 0.2083333)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

# monotinicity
test_that("examples work", {
  y = c(0, 1, 2, 1, 4)
  n = c(4, 4, 4, 4, 4)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 3.5,
                var  = 0.2083333)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

# uneven
test_that("examples work", {
  y = c(0, 2, 3, 4)
  n = c(4, 4, 4, 4)
  x = c(1, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 3,
                var  = 0.3958333)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

# bracket
test_that("examples work", {
  y = c(0, 1, 2, 3, 4)
  n = c(8, 8, 8, 8, 8)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 4.25,
                var  = 0.1116071)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})
  
test_that("examples work", {
  y = c(4, 5, 6, 7, 8)
  n = c(8, 8, 8, 8, 8)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 1.75,
                var  = 0.1116071)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

# monotincity + reversed
test_that("examples work", {
  y = c(4, 1, 2, 1, 0)
  n = c(4, 4, 4, 4, 4)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 2.5,
                var  = 0.2083333)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

# bracket + uneven
test_that("examples work", {
  y = c(0, 1, 2, 4)
  n = c(8, 8, 8, 8)
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 4.5,
                var  = 0.1852679)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

test_that("examples work", {
  y = c(4, 5, 6, 8)
  n = c(8, 8, 8, 8)
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 1.75,
                var  = 0.09598214)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

# monotinicity + bracket
test_that("examples work", {
  y = c(0, 1, 2, 1, 4)
  n = c(8, 8, 8, 8, 8)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 4.5,
                var  = 0.09375)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

#  + reversed
test_that("examples work", {
  y = c(4, 1, 2, 1, 0) + 4
  n = c(8, 8, 8, 8, 8)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 4,
                var  = 0.1294643)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

# monotinicity + uneven
test_that("examples work", {
  y = c(0, 2, 1, 4)
  n = c(4, 4, 4, 4)
  x = c(1, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 3.5,
                var  = 0.3958333)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

#   + reversed
test_that("examples work", {
  y = c(4, 1, 2, 0)
  n = c(4, 4, 4, 4)
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 2.5,
                var  = 0.3958333)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

# monotinicity + uneven + bracket
test_that("examples work", {
  y = c(0, 2, 1, 4)
  n = c(4, 4, 4, 4) + 4
  x = c(1, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 4.5,
                var  = 0.1584821)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

test_that("examples work", {
  y = c(4, 1, 2, 0)
  n = c(4, 4, 4, 4) + 4
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 1.5,
                var  = 0.1584821)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

test_that("examples work", {
  y = c(0, 2, 1, 4) + 4
  n = c(4, 4, 4, 4) + 4
  x = c(1, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 1.75,
                var  = 0.2834821)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

test_that("examples work", {
  y = c(4, 1, 2, 0) + 4
  n = c(4, 4, 4, 4) + 4
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = SpearKarb(y = y, x = x, n = n)
  resb = SpearKarb(y + n ~ x)
  resc = SpearKarb(dead + total ~ dil, dt)
  res  = list(eval = "Spearman-Kärber",
                ed   = 4.25,
                var  = 0.2834821)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_identical(resa$eval, res$eval)
  expect_identical(resb$eval, res$eval)
  expect_identical(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
  expect_equal(resa$var, res$var, tol = 0.001)
  expect_equal(resb$var, res$var, tol = 0.001)
  expect_equal(resc$var, res$var, tol = 0.001)
})

