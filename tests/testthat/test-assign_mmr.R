data("emdb")

test_that("mmr2 works", {
  emdb$mmr2 <- assign_mmr2(
    msh6 = emdb$msh6,
    pms2 = emdb$pms2
  )
  expect_equal(sum(emdb$mmr2 == "intact"), 578)
  expect_equal(sum(emdb$mmr2 == "deficient"), 222)
})

test_that("mmr4 works", {
  emdb$mmr4 <- assign_mmr4(
    msh6 = emdb$msh6,
    pms2 = emdb$pms2,
    mlh1 = emdb$mlh1,
    msh2 = emdb$msh2
  )
  expect_equal(sum(emdb$mmr4 == "intact"), 195)
  expect_equal(sum(emdb$mmr4 == "deficient"), 605)
})
