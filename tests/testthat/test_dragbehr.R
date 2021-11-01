context("DragBehr")

#All Good
test_that("examples work", {
  y = c(0, 1, 2, 3, 4)
  n = c(4, 4, 4, 4, 4)
  x = c(1, 2, 3, 4, 5)
  dt = data.frame(dead = y, total = n, dil = x)
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 3.6363636)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 3.2307692308)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 4.318181818)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 1.68181818)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 2.36363636)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 4.327586206897)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 1.5737704918)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 4.5833333)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 4.105263157895)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 4.676923076923)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 1.32307692308)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 2.071428571429)
  
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
  resa = DragBehr(y = y, x = x, n = n)
  resb = DragBehr(y + n ~ x)
  resc = DragBehr(dead + total ~ dil, dt)
  res  = list(eval = "Dragstedt-Behrens",
                ed   = 3.928571428571)
  
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

