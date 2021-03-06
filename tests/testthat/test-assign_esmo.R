data("emdb")

test_that("esmo2013 works", {
  emdb$esmo2013 <- assign_esmo2013(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr
  )
  expect_equal(sum(emdb$esmo2013 == "low", na.rm = TRUE), 89)
  expect_equal(sum(emdb$esmo2013 == "intermediate", na.rm = TRUE), 117)
  expect_equal(sum(emdb$esmo2013 == "high", na.rm = TRUE), 425)
})

test_that("esmo2016 works", {
  emdb$esmo2016 <- assign_esmo2016(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr,
    myo = emdb$myo,
    lvi = emdb$lvi
  )
  expect_equal(sum(emdb$esmo2016 == "low", na.rm = TRUE), 36)
  expect_equal(sum(emdb$esmo2016 == "intermediate", na.rm = TRUE), 21)
  expect_equal(sum(emdb$esmo2016 == "high-intermediate", na.rm = TRUE), 99)
  expect_equal(sum(emdb$esmo2016 == "high", na.rm = TRUE), 421)
  expect_equal(sum(emdb$esmo2016 == "advanced", na.rm = TRUE), 12)
  expect_equal(sum(emdb$esmo2016 == "metastatic", na.rm = TRUE), 6)
})

test_that("esmo2016 with residual works", {
  emdb$esmo2016 <- assign_esmo2016(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr,
    myo = emdb$myo,
    lvi = emdb$lvi,
    residual = emdb$residual
  )
  expect_equal(sum(emdb$esmo2016 == "low", na.rm = TRUE), 36)
  expect_equal(sum(emdb$esmo2016 == "intermediate", na.rm = TRUE), 21)
  expect_equal(sum(emdb$esmo2016 == "high-intermediate", na.rm = TRUE), 99)
  expect_equal(sum(emdb$esmo2016 == "high", na.rm = TRUE), 398)
  expect_equal(sum(emdb$esmo2016 == "advanced", na.rm = TRUE), 35)
  expect_equal(sum(emdb$esmo2016 == "metastatic", na.rm = TRUE), 6)
})

test_that("esmo2020 works", {
  emdb$esmo2020 <- assign_esmo2020(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr,
    myo = emdb$myo,
    lvi = emdb$lvi
  )
  expect_equal(sum(emdb$esmo2020 == "low", na.rm = TRUE), 51)
  expect_equal(sum(emdb$esmo2020 == "intermediate", na.rm = TRUE), 100)
  expect_equal(sum(emdb$esmo2020 == "high-intermediate", na.rm = TRUE), 130)
  expect_equal(sum(emdb$esmo2020 == "high", na.rm = TRUE), 285)
  expect_equal(sum(emdb$esmo2020 == "advanced", na.rm = TRUE), 17)
  expect_equal(sum(emdb$esmo2020 == "metastatic", na.rm = TRUE), 6)
})

test_that("esmo2020 with eclass works", {
  emdb$promise2015 <- assign_promise2015(
    mmr = emdb$mmr_ihc_2,
    pole = emdb$pole_mut,
    p53 = emdb$p53
  )
  emdb$esmo2020 <- assign_esmo2020(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr,
    myo = emdb$myo,
    lvi = emdb$lvi,
    eclass = emdb$promise2015
  )
  expect_equal(sum(emdb$esmo2020 == "low", na.rm = TRUE), 75)
  expect_equal(sum(emdb$esmo2020 == "intermediate", na.rm = TRUE), 53)
  expect_equal(sum(emdb$esmo2020 == "high-intermediate", na.rm = TRUE), 89)
  expect_equal(sum(emdb$esmo2020 == "high", na.rm = TRUE), 265)
  expect_equal(sum(emdb$esmo2020 == "advanced", na.rm = TRUE), 38)
  expect_equal(sum(emdb$esmo2020 == "metastatic", na.rm = TRUE), 6)
})

test_that("esmo2020 with residual disease works", {
  emdb$promise2015 <- assign_promise2015(
    mmr = emdb$mmr_ihc_2,
    pole = emdb$pole_mut,
    p53 = emdb$p53
  )
  emdb$esmo2020 <- assign_esmo2020(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr,
    myo = emdb$myo,
    lvi = emdb$lvi,
    eclass = emdb$promise2015,
    residual = emdb$residual
  )
  expect_equal(sum(emdb$esmo2020 == "low", na.rm = TRUE), 70)
  expect_equal(sum(emdb$esmo2020 == "intermediate", na.rm = TRUE), 53)
  expect_equal(sum(emdb$esmo2020 == "high-intermediate", na.rm = TRUE), 89)
  expect_equal(sum(emdb$esmo2020 == "high", na.rm = TRUE), 239)
  expect_equal(sum(emdb$esmo2020 == "advanced", na.rm = TRUE), 32)
  expect_equal(sum(emdb$esmo2020 == "metastatic", na.rm = TRUE), 6)
})
