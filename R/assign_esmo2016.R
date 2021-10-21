#' Assign ESMO 2016
#'
#' Assign ESMO 2016 based on stage, grade, histological subtype group,
#' myometrial invasion, and LVSI. Residual disease can be used if available.
#'
#' ESMO 2016 is assigned using stage, grade, histological subtype group,
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
#' @param residual residual disease: "<1cm", ">=1cm", "microscopic", or "no
#'   residual". Use if available. Microscopic residual is regarded as no
#'   residual disease.
#' @note Assignment starts from the metastatic group first as the criteria are
#'   not mutually exclusive.
#' @return ESMO 2016 assigned into "low", "intermediate", "high-intermediate",
#'   "high", "advanced", or "metastatic". Unassignable cases are `NA`.
#' @references Colombo et al. Annals of Oncology 2016
#' @author Derek Chiu, Samuel Leung
#' @export
#' @examples
#' esmo2016 <- with(emdb, assign_esmo2016(stage_full, grade_rev, hist_rev_gr,
#' myo, lvi, residual))
#' table(esmo2016)
assign_esmo2016 <- function(stage_full, grade, hist_gr, myo, lvi,
                            residual = NULL) {
  # Validate inputs
  check_input(stage_full, STAGE_STD)
  check_input(grade, GRADE_STD)
  check_input(hist_gr, HIST_STD)
  check_input(myo, MYO_STD)
  check_input(lvi, LVI_STD)

  # Validate residual disease if provided
  if (is.null(residual)) {
    no_residual <- TRUE
  } else {
    no_residual <- FALSE
    check_input(residual, RESIDUAL_STD)
  }

  # Assign risk groups
  esmo2016 <- dplyr::case_when(
    # metastatic
    stage_full == "IVB" ~ VC.METASTATIC,

    # advanced
    stage_full == "IVA" ~ VC.ADVANCED,

    if (!no_residual) {
      stage_full %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2") &
        !residual %in% c("no residual", "microscopic")
    } ~ VC.ADVANCED,

    # high
    (grepl("^I[A-C]?$", stage_full) &
       hist_gr == "endometrioid" &
       grade == "grade 3" & myo == ">50%") |
      (grepl("^(II|III|IV)[A-C]?[1-2]?$", stage_full)) |
      (hist_gr == "non-endometrioid") ~ VC.HIGH,

    if (!no_residual) {
      (stage_full %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2") &
         hist_gr == "endometrioid" &
         residual %in% c("no residual", "microscopic")) |
        (hist_gr == "non-endometrioid" &
           residual %in% c("no residual", "microscopic"))
    } ~ VC.HIGH,

    # high-intermediate
    (grepl("^I[A-C]?$", stage_full) &
       hist_gr == "endometrioid" &
       grade == "grade 3" & myo %in% c(VC.NONE, "1-50%")) |
      (grepl("^I[A-C]?$", stage_full) &
         hist_gr == "endometrioid" &
         grade %in% c("grade 1", "grade 2") & lvi == VC.POSITIVE) ~ VC.HIGH.INTERM,

    # intermediate
    grepl("^I[A-C]?$", stage_full) &
      hist_gr == "endometrioid" &
      grade %in% c("grade 1", "grade 2") &
      myo == ">50%" & lvi == VC.NEGATIVE ~ VC.INTERM,

    # low
    grepl("^I[A-C]?$", stage_full) &
      hist_gr == "endometrioid" &
      grade %in% c("grade 1", "grade 2") &
      myo %in% c(VC.NONE, "1-50%") & lvi == VC.NEGATIVE ~ VC.LOW,

    # unassignable
    TRUE ~ NA_character_
  )

  # Set factor level order
  factor(esmo2016, levels = c(VC.LOW, VC.INTERM, VC.HIGH.INTERM, VC.HIGH, VC.ADVANCED, VC.METASTATIC))
}
