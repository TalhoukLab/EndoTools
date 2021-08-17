#' Assign ESMO 2016
#'
#' Assign ESMO 2016 based on stage, grade, histotype, myometrial invasion, and
#' LVSI.
#'
#' ESMO 2013 is assigned using stage, grade, histotype, myometrial invasion, and
#' LVSI into low, intermediate, high-intermediate, high, advanced, and
#' metastatic risk based on the following criteria:
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
#' @param myo myometrial invasion
#' @param lvi lymphovascular space invasion
#' @note Assignment starts from the metastatic group first as the criteria are
#'   not mutually exclusive. Residual disease is not available and so are not
#'   used in criteria for high-intermediate and high.
#' @return ESMO 2016 assigned into "low", "intermediate", "high-intermediate",
#'   "high", "advanced", or "metastatic". Unassignable cases are `NA`.
#' @references Colombo et al. Annals of Oncology 2016
#' @author Derek Chiu, Samuel Leung
#' @export
assign_esmo2016 <- function(stage, grade, hist, myo, lvi) {
  # metastatic
  if (stage == "IVB") {
    return(VC.METASTATIC)
  }

  # advanced
  if (stage == "IVA") {
    return(VC.ADVANCED)
  }

  # high
  if ((stage %in% all_stage1 &
       hist == "endometrioid" &
       grade == "grade 3" & myo == ">50%") |
      (stage %in% stage_2_or_higher) |
      (hist == "non-endometrioid")) {
    return(VC.HIGH)
  }

  # high-intermediate
  if ((
    stage %in% all_stage1 &
    hist == "endometrioid" &
    grade == "grade 3" & myo %in% c(VC.NONE, "1-50%")
  ) |
  (
    stage %in% all_stage1 &
    hist == "endometrioid" &
    grade %in% c("grade 1", "grade 2") & lvi == VC.POSITIVE
  )) {
    return(VC.HIGH.INTERM)
  }

  # intermediate
  if (stage %in% all_stage1 &
      hist == "endometrioid" &
      grade %in% c("grade 1", "grade 2") &
      myo == ">50%" & lvi == VC.NEGATIVE) {
    return(VC.INTERM)
  }

  # low
  if (stage %in% all_stage1 &
      hist == "endometrioid" &
      grade %in% c("grade 1", "grade 2") &
      myo %in% c(VC.NONE, "1-50%") & lvi == VC.NEGATIVE) {
    return(VC.LOW)
  }

  # unassignable
  return(NA)
}
