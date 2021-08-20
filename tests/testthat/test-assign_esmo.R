data("emdb")

test_that("esmo2013 works", {
  emdb$esmo2013 <- assign_esmo2013(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr
  )
  expect_equal(sum(emdb$esmo2013 == "low"), 168)
  expect_equal(sum(emdb$esmo2013 == "intermediate"), 187)
  expect_equal(sum(emdb$esmo2013 == "high"), 445)
})

test_that("esmo2016 works", {
  emdb$esmo2016 <- assign_esmo2016(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr,
    myo = emdb$myo,
    lvi = emdb$lvi
  )
  expect_equal(sum(emdb$esmo2016 == "low"), 88)
  expect_equal(sum(emdb$esmo2016 == "intermediate"), 56)
  expect_equal(sum(emdb$esmo2016 == "high-intermediate"), 174)
  expect_equal(sum(emdb$esmo2016 == "high"), 464)
  expect_equal(sum(emdb$esmo2016 == "advanced"), 12)
  expect_equal(sum(emdb$esmo2016 == "metastatic"), 6)
})

test_that("esmo2020 works", {
  emdb$esmo2020 <- assign_esmo2020(
    stage = emdb$stage_full,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr,
    hist = emdb$hist,
    myo = emdb$myo,
    lvi = emdb$lvi
  )
  expect_equal(sum(emdb$esmo2020 == "low", na.rm = TRUE), 110)
  expect_equal(sum(emdb$esmo2020 == "intermediate", na.rm = TRUE), 124)
  expect_equal(sum(emdb$esmo2020 == "high-intermediate", na.rm = TRUE), 181)
  expect_equal(sum(emdb$esmo2020 == "high", na.rm = TRUE), 156)
  expect_equal(sum(emdb$esmo2020 == "advanced", na.rm = TRUE), 45)
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
    hist = emdb$hist,
    myo = emdb$myo,
    lvi = emdb$lvi,
    eclass = emdb$promise2015
  )
  expect_equal(sum(emdb$esmo2020 == "low", na.rm = TRUE), 134)
  expect_equal(sum(emdb$esmo2020 == "intermediate", na.rm = TRUE), 89)
  expect_equal(sum(emdb$esmo2020 == "high-intermediate", na.rm = TRUE), 144)
  expect_equal(sum(emdb$esmo2020 == "high", na.rm = TRUE), 188)
  expect_equal(sum(emdb$esmo2020 == "advanced", na.rm = TRUE), 61)
  expect_equal(sum(emdb$esmo2020 == "metastatic", na.rm = TRUE), 6)
})
