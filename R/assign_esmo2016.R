#' Assign ESMO 2016
#'
#' Assign ESMO 2016 based on stage, grade, histological subtype group,
#' myometrial invasion, and LVSI.
#'
#' ESMO 2013 is assigned using stage, grade, histological subtype group,
#' myometrial invasion, and LVSI into low, intermediate, high-intermediate,
#' high, advanced, and metastatic risk based on the following criteria:
#' * low:
#'   * stage I, grade 1/2, endometrioid, <50% myometrial invasion, LVSI negative
#' * intermediate:
#'   * stage I, grade 1/2, endometrioid, >=50% myometrial invasion, LVSI negative
#' * high-intermediate:
#'   * stage I, grade 3, endometrioid, <50% myometrial invasion, regardless of LVSI status
#'   * stage I, grade 1/2, endometrioid, LVSI unequivocally positive, regardless of depth of invasion
#' * high:
#'   * stage I, grade 3, endometrioid, >=50% myometrial invasion, regardless of LVSI status
#'   * stage II or higher
#'   * stage III, endometrioid, no residual disease
#'   * non-endometrioid (serous or clear-cell or undifferentiated carcinoma, or carcinosarcoma)
#' * advanced:
#'   * stage III residual disease
#'   * stage IVA
#' * metastatic:
#'   * stage IVB
#'
#' @inheritParams assign_esmo2013
#' @param myo myometrial invasion: none, 1-50%, >50%
#' @param lvi lymphovascular space invasion: negative, positive, focal, extensive
#' @note Assignment starts from the metastatic group first as the criteria are
#'   not mutually exclusive. Residual disease is not available and so are not
#'   used in criteria for high-intermediate and high.
#' @return ESMO 2016 assigned into "low", "intermediate", "high-intermediate",
#'   "high", "advanced", or "metastatic". Unassignable cases are `NA`.
#' @references Colombo et al. Annals of Oncology 2016
#' @author Derek Chiu, Samuel Leung
#' @export
#' @examples
#' esmo2016 <- with(emdb, assign_esmo2016(stage_full, grade_rev, hist_rev_gr,
#' myo, lvi))
#' table(esmo2016)
assign_esmo2016 <- function(stage, grade, hist_gr, myo, lvi) {
  dplyr::case_when(
    # metastatic
    stage == "IVB" ~ VC.METASTATIC,

    # advanced
    stage == "IVA" ~ VC.ADVANCED,

    # high
    (grepl("^I[A-C]?$", stage) &
       hist_gr == "endometrioid" &
       grade == "grade 3" & myo == ">50%") |
      (grepl("^(II|III|IV)[A-C]?[1-2]?$", stage)) |
      (hist_gr == "non-endometrioid") ~ VC.HIGH,

    # high-intermediate
    (grepl("^I[A-C]?$", stage) &
       hist_gr == "endometrioid" &
       grade == "grade 3" & myo %in% c(VC.NONE, "1-50%")) |
      (grepl("^I[A-C]?$", stage) &
         hist_gr == "endometrioid" &
         grade %in% c("grade 1", "grade 2") & lvi == VC.POSITIVE) ~ VC.HIGH.INTERM,

    # intermediate
    grepl("^I[A-C]?$", stage) &
      hist_gr == "endometrioid" &
      grade %in% c("grade 1", "grade 2") &
      myo == ">50%" & lvi == VC.NEGATIVE ~ VC.INTERM,

    # low
    grepl("^I[A-C]?$", stage) &
      hist_gr == "endometrioid" &
      grade %in% c("grade 1", "grade 2") &
      myo %in% c(VC.NONE, "1-50%") & lvi == VC.NEGATIVE ~ VC.LOW,

    # unassignable
    TRUE ~ NA_character_
  )
}
