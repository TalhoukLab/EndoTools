data("emdb")

test_that("promise2015 works", {
  emdb$promise2015 <- assign_promise2015(
    mmr_ihc_2 = emdb$mmr_ihc_2,
    pole_mut = emdb$pole_mut,
    p53 = emdb$p53
  )
  expect_equal(sum(emdb$promise2015 == "MMRd"), 196)
  expect_equal(sum(emdb$promise2015 == "POLEmut"), 67)
  expect_equal(sum(emdb$promise2015 == "p53abn"), 119)
  expect_equal(sum(emdb$promise2015 == "NSMP/p53wt"), 418)
})

test_that("promise2019 works", {
  emdb$promise2019 <- assign_promise2019(
    mmr_ihc_2 = emdb$mmr_ihc_2,
    pole_mut = emdb$pole_mut,
    p53 = emdb$p53
  )
  expect_equal(sum(emdb$promise2019 == "POLEmut"), 63)
  expect_equal(sum(emdb$promise2019 == "MMRd"), 182)
  expect_equal(sum(emdb$promise2019 == "p53abn"), 124)
  expect_equal(sum(emdb$promise2019 == "NSMP/p53wt"), 431)
})

test_that("promisepoor works", {
  emdb$promisepoor <- assign_promisepoor(
    mmr_ihc_2 = emdb$mmr_ihc_2,
    mmr_ihc_4 = emdb$mmr_ihc_4,
    pole_mut = emdb$pole_mut,
    p53 = emdb$p53,
    grade = emdb$grade_rev,
    hist_gr = emdb$hist_rev_gr
  )
  expect_equal(sum(emdb$promisepoor == "Low-Risk"), 190)
  expect_equal(sum(emdb$promisepoor == "POLEmut"), 49)
  expect_equal(sum(emdb$promisepoor == "MMRd"), 182)
  expect_equal(sum(emdb$promisepoor == "p53abn"), 124)
  expect_equal(sum(emdb$promisepoor == "NSMP/p53wt"), 255)
})

test_that("p53 can be used raw or consolidated", {
  emdb$promise2015_v1 <- assign_promise2015(
    mmr_ihc_2 = emdb$mmr_ihc_2,
    pole_mut = emdb$pole_mut,
    p53 = emdb$p53
  )
  emdb$promise2015_v2 <- assign_promise2015(
    mmr_ihc_2 = emdb$mmr_ihc_2,
    pole_mut = emdb$pole_mut,
    p53 = emdb$p53_mut
  )
  expect_identical(emdb$promise2015_v1, emdb$promise2015_v2)
})
