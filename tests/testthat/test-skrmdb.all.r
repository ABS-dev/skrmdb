test_that(".skrmdb_all", {
  expect_true(TRUE)
})

test_that("skrmdb.all", {
  dat         <- titration
  dat$log_dil <- -log10(dat$dil)
  res_all     <- skrmdb.all(dat, positive + total ~ log_dil | Operator + Vial)
  res_sk      <- SpearKarb(dat, positive + total ~ log_dil | Operator + Vial)
  res_db      <- DragBehr(dat, positive + total ~ log_dil | Operator + Vial)
  res_rm      <- ReedMuench(dat, positive + total ~ log_dil | Operator + Vial)
  res_all     <- res_all$results
  res_sk      <- res_sk$results
  res_db      <- res_db$results
  res_rm      <- res_rm$results

  expect_equal(res_rm$ed,  res_all$ReedMuench)
  expect_equal(res_db$ed,  res_all$DragBehr)
  expect_equal(res_sk$ed,  res_all$SpearKarb)
  expect_equal(res_sk$var, res_all$SpearKarb.var)
})
