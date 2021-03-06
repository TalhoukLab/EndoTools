test_that("ace27 works", {
  emdb$ace27 <- assign_ace27(
    ace_mi = emdb$ace_mi,
    ace_chf = emdb$ace_chf,
    ace_cad = emdb$ace_cad,
    ace_arr = emdb$ace_arr,
    ace_htn = emdb$ace_htn,
    ace_vd = emdb$ace_vd,
    ace_pad = emdb$ace_pad,
    ace_res = emdb$ace_res,
    ace_hep = emdb$ace_hep,
    ace_sto = emdb$ace_sto,
    ace_pan = emdb$ace_pan,
    ace_rd = emdb$ace_rd,
    ace_dm = emdb$ace_dm,
    ace_str = emdb$ace_str,
    ace_dem = emdb$ace_dem,
    ace_par = emdb$ace_par,
    ace_neu = emdb$ace_neu,
    ace_psy = emdb$ace_psy,
    ace_rhe = emdb$ace_rhe,
    ace_aid = emdb$ace_aid,
    ace_st = emdb$ace_st,
    ace_lm = emdb$ace_lm,
    ace_lym = emdb$ace_lym,
    ace_alc = emdb$ace_alc,
    ace_id = emdb$ace_id,
    ace_obe = emdb$ace_obe
  )
  expect_equal(sum(emdb$ace27 == "None", na.rm = TRUE), 111)
  expect_equal(sum(emdb$ace27 == ">=Mild", na.rm = TRUE), 441)
  expect_equal(sum(emdb$ace27 == ">=Moderate", na.rm = TRUE), 177)
  expect_equal(sum(emdb$ace27 == "Severe", na.rm = TRUE), 71)
})

test_that("NOS can be separated", {
  emdb$ace27 <- assign_ace27(
    ace_mi = emdb$ace_mi,
    ace_chf = emdb$ace_chf,
    ace_cad = emdb$ace_cad,
    ace_arr = emdb$ace_arr,
    ace_htn = emdb$ace_htn,
    ace_vd = emdb$ace_vd,
    ace_pad = emdb$ace_pad,
    ace_res = emdb$ace_res,
    ace_hep = emdb$ace_hep,
    ace_sto = emdb$ace_sto,
    ace_pan = emdb$ace_pan,
    ace_rd = emdb$ace_rd,
    ace_dm = emdb$ace_dm,
    ace_str = emdb$ace_str,
    ace_dem = emdb$ace_dem,
    ace_par = emdb$ace_par,
    ace_neu = emdb$ace_neu,
    ace_psy = emdb$ace_psy,
    ace_rhe = emdb$ace_rhe,
    ace_aid = emdb$ace_aid,
    ace_st = emdb$ace_st,
    ace_lm = emdb$ace_lm,
    ace_lym = emdb$ace_lym,
    ace_alc = emdb$ace_alc,
    ace_id = emdb$ace_id,
    ace_obe = emdb$ace_obe,
    separate_nos = TRUE
  )
  expect_equal(sum(emdb$ace27 == "None", na.rm = TRUE), 111)
  expect_equal(sum(emdb$ace27 == "Mild", na.rm = TRUE), 331)
  expect_equal(sum(emdb$ace27 == "Moderate", na.rm = TRUE), 177)
  expect_equal(sum(emdb$ace27 == "Severe", na.rm = TRUE), 71)
  expect_equal(sum(emdb$ace27 == "NOS", na.rm = TRUE), 110)
})

test_that("assign_ace_vars gives list of numeric scores", {
  ace_list <- assign_ace_vars(
    ace_mi = emdb$ace_mi,
    ace_chf = emdb$ace_chf,
    ace_cad = emdb$ace_cad,
    ace_arr = emdb$ace_arr,
    ace_htn = emdb$ace_htn,
    ace_vd = emdb$ace_vd,
    ace_pad = emdb$ace_pad,
    ace_res = emdb$ace_res,
    ace_hep = emdb$ace_hep,
    ace_sto = emdb$ace_sto,
    ace_pan = emdb$ace_pan,
    ace_rd = emdb$ace_rd,
    ace_dm = emdb$ace_dm,
    ace_str = emdb$ace_str,
    ace_dem = emdb$ace_dem,
    ace_par = emdb$ace_par,
    ace_neu = emdb$ace_neu,
    ace_psy = emdb$ace_psy,
    ace_rhe = emdb$ace_rhe,
    ace_aid = emdb$ace_aid,
    ace_st = emdb$ace_st,
    ace_lm = emdb$ace_lm,
    ace_lym = emdb$ace_lym,
    ace_alc = emdb$ace_alc,
    ace_id = emdb$ace_id,
    ace_obe = emdb$ace_obe
  )
  expect_length(ace_list, 26)
  expect_type(unlist(ace_list), "double")
  expect_equal(range(unlist(ace_list), na.rm = TRUE), c(0, 3))
})
