#' Assign ESMO 2013
#'
#' Assign ESMO 2013 based on stage, grade, and histological subtype group
#'
#' ESMO 2013 is assigned using stage, grade, and histological subtype group into
#' low, intermediate, and high risk based on the following criteria:
#' * low:
#'   * stage IA, grade 1/2, with endometrioid type
#' * intermediate:
#'   * stage IA, grade 3, with endometrioid type
#'   * stage IB, grade 1/2, with endometrioid type
#' * high:
#'   * all stages with non-endometrioid type
#'   * stage IB, grade 3, with endometrioid type
#'   * stage II or higher
#'
#' @param stage_full FIGO stage: I, II, III, IV with substages
#' @param grade tumour grade: 1, 2, 3
#' @param hist_gr histological subtype group: endometrioid or non-endometrioid
#' @note Assignment starts from the high group first as the criteria are not
#'   mutually exclusive.
#' @return ESMO 2013 assigned into "low", "intermediate", or "high".
#'   Unassignable cases are `NA`.
#' @references Colombo et al. Ann Oncol 2013
#' @author Derek Chiu, Samuel Leung
#' @export
#' @examples
#' esmo2013 <- with(emdb, assign_esmo2013(stage_full, grade_rev, hist_rev_gr))
#' table(esmo2013)
assign_esmo2013 <- function(stage_full, grade, hist_gr) {
  dplyr::case_when(
    # high
    (hist_gr == "non-endometrioid") |
      (grepl("^(IB|IC)", stage_full) &
         grade == "grade 3" & hist_gr == "endometrioid") |
      (grepl("^(II|III|IV)[A-C]?[1-2]?$", stage_full)) ~ VC.HIGH,

    # intermediate
    (stage_full %in% c("I", "IA") &
       grade == "grade 3" & hist_gr == "endometrioid") |
      (grepl("^(IB|IC)", stage_full) &
         grade %in% c("grade 1", "grade 2") & hist_gr == "endometrioid") ~ VC.INTERM,

    # low
    stage_full %in% c("I", "IA") &
      grade %in% c("grade 1", "grade 2") & hist_gr == "endometrioid" ~ VC.LOW,

    # unassignable
    TRUE ~ NA_character_
  )
}
