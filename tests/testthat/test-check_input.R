STAGE_STD <- c("I", "IA", "IB", "IC", "II", "IIA", "IIB", "III", "IIIA",
               "IIIB", "IIIC", "IIIC1", "IIIC2", "IV", "IVA", "IVB")
tmp <- emdb$stage_full

test_that("check_input works for valid inputs", {
  expect_true(check_input(tmp, STAGE_STD))
})

test_that("check_input throws error for invalid inputs", {
  levels(tmp) <- c(levels(tmp), "haha")
  tmp[1] <- "haha"
  expect_error(check_input(tmp, STAGE_STD))
})
