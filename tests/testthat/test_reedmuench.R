context("ReedMuench")

#All Good
test_that("examples work", {
  y = c(0, 1, 2, 3, 4)
  n = c(4, 4, 4, 4, 4)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 3)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

# monotinicity
test_that("examples work", {
  y = c(0, 1, 2, 1, 4)
  n = c(4, 4, 4, 4, 4)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 3.66666666666)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

# uneven
test_that("examples work", {
  y = c(0, 2, 3, 4)
  n = c(4, 4, 4, 4)
  x = c(1, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 3.2)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

# bracket
test_that("examples work", {
  y = c(0, 1, 2, 3, 4)
  n = c(8, 8, 8, 8, 8)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 4.33333333)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})
  
test_that("examples work", {
  y = c(4, 5, 6, 7, 8)
  n = c(8, 8, 8, 8, 8)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 1.66666666666)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

# monotincity + reversed
test_that("examples work", {
  y = c(4, 1, 2, 1, 0)
  n = c(4, 4, 4, 4, 4)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 2.3333333333)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

# bracket + uneven
test_that("examples work", {
  y = c(0, 1, 2, 4)
  n = c(8, 8, 8, 8)
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 4.4)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

test_that("examples work", {
  y = c(4, 5, 6, 8)
  n = c(8, 8, 8, 8)
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 1.555555555)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

# monotinicity + bracket
test_that("examples work", {
  y = c(0, 1, 2, 1, 4)
  n = c(8, 8, 8, 8, 8)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 4.63636363636)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

#  + reversed
test_that("examples work", {
  y = c(4, 1, 2, 1, 0) + 4
  n = c(8, 8, 8, 8, 8)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 4.1111111111)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

# monotinicity + uneven
test_that("examples work", {
  y = c(0, 2, 1, 4)
  n = c(4, 4, 4, 4)
  x = c(1, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 4)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

#   + reversed
test_that("examples work", {
  y = c(4, 1, 2, 0)
  n = c(4, 4, 4, 4)
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 2)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

# monotinicity + uneven + bracket
test_that("examples work", {
  y = c(0, 2, 1, 4)
  n = c(4, 4, 4, 4) + 4
  x = c(1, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 4.72727272)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

test_that("examples work", {
  y = c(4, 1, 2, 0)
  n = c(4, 4, 4, 4) + 4
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 1.272727272)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

test_that("examples work", {
  y = c(0, 2, 1, 4) + 4
  n = c(4, 4, 4, 4) + 4
  x = c(1, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 2)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

test_that("examples work", {
  y = c(4, 1, 2, 0) + 4
  n = c(4, 4, 4, 4) + 4
  x = c(1, 2, 3, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = ReedMuench(y = y, x = x, n = n)
  resb = ReedMuench(y + n ~ x)
  resc = ReedMuench(dead + total ~ dil, dt)
  res  = list(eval = "Reed-Muench",
                ed   = 4)
  
  expect_s3_class(resa, "skrmdb")
  expect_s3_class(resb, "skrmdb")
  expect_s3_class(resc, "skrmdb")
  expect_equal(resa$eval, res$eval)
  expect_equal(resb$eval, res$eval)
  expect_equal(resc$eval, res$eval)
  expect_equal(resa$ed, res$ed)
  expect_equal(resb$ed, res$ed)
  expect_equal(resc$ed, res$ed)
})

