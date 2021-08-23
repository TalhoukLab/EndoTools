#' Assign ESMO 2020
#'
#' Assign ESMO 2020 based on stage, grade, histological subtype group,
#' histological subtype, myometrial invasion, and LVSI. Molecular classification
#' can be used if available.
#'
#' ESMO 2020 is assigned using stage, grade, histological subtype group,
#' histological subtype, myometrial invasion, and LVSI into low, intermediate,
#' high-intermediate, high, advanced, and metastatic risk based on the following
#' criteria with and without molecular classification:
#'
#' **With molecular classification**
#' * low:
#'   * stage I-II, POLEmut
#'   * stage IA, grade 1/2, endometrioid, <50% myometrial invasion, LVSI negative or focal, MMRd/NSMP
#' * intermediate:
#'   * stage IB, grade 1/2, endometrioid, <50% myometrial invasion, LVSI negative or focal, MMRd/NSMP
#'   * stage IA, grade 3, endometrioid, LVSI negative or focal, MMRd/NSMP
#'   * stage IA, non-endometrioid or mixed subtype (serous, carcinosarcoma (MMMT), or undifferentiated), no myometrial invasion, p53abn
#' * high-intermediate:
#'   * stage IA/IB, endometrioid, LVSI positive or extensive, MMRd/NSMP
#'   * stage IB, grade 3, endometrioid, MMRd/NSMP
#'   * stage II/IIA, endometrioid, MMRd/NSMP
#' * high:
#'   * stage III-IVA, endometrioid, MMRd/NSMP
#'   * stage I-IVA, endometrioid, >0% myometrial invasion, p53abn
#'   * stage IA, non-endometrioid or mixed subtype (serous, carcinosarcoma (MMMT), or undifferentiated), >0% myometrial invasion, MMRd/NSMP
#'   * stage IB-IVA, non-endometrioid or mixed subtype (serous, carcinosarcoma (MMMT), or undifferentiated), MMRd/NSMP
#' * advanced:
#'   * stage III-IVA, any molecular type
#' * metastatic:
#'   * stage IVB, any molecular type
#'
#' **Without molecular classification**
#' * low:
#'   * stage IA, grade 1/2, endometrioid, <50% myometrial invasion, LVSI negative or focal
#' * intermediate:
#'   * stage IB, grade 1/2, endometrioid, <50% myometrial invasion, LVSI negative or focal
#'   * stage IA, grade 3, endometrioid, LVSI negative or focal
#'   * stage IA, non-endometrioid or mixed subtype (serous, carcinosarcoma (MMMT), or undifferentiated), no myometrial invasion
#' * high-intermediate:
#'   * stage IA/IB, endometrioid, LVSI positive or extensive
#'   * stage IB, grade 3, endometrioid
#'   * stage II/IIA, endometrioid
#' * high:
#'   * stage III-IVA, endometrioid
#'   * stage IA, non-endometrioid or mixed subtype (serous, carcinosarcoma (MMMT), or undifferentiated), >0% myometrial invasion
#'   * stage IB-IVA, non-endometrioid or mixed subtype (serous, carcinosarcoma (MMMT), or undifferentiated)
#' * advanced:
#'   * stage III-IVA
#' * metastatic:
#'   * stage IVB
#' @inheritParams assign_esmo2016
#' @param hist histological subtype: detailed histotype
#' @param eclass molecular classification: "MMRd", "POLEmut", "p53abn", or
#'   "NSMP/p53wt"
#' @note Assignment starts from the low group first.
#' @return ESMO 202020 assigned into "low", "intermediate", "high-intermediate",
#'   "high", "advanced", or "metastatic". Unassignable cases are `NA`.
#' @references Concin N, Matias-Guiu X, Vergote I, et al ESGO/ESTRO/ESP
#'   guidelines for the management of patients with endometrial
#'   carcinoma. International Journal of Gynecologic Cancer Published Online
#'   First: 18 December 2020. doi: 10.1136/ijgc-2020-002230
#' @author Derek Chiu
#' @export
#' @examples
#' # without molecular classification
#' esmo2020_wo_eclass <- with(emdb, assign_esmo2020(stage_full, grade_rev,
#' hist_rev_gr, hist, myo, lvi))
#' table(esmo2020_wo_eclass)
#'
#' # with molecular classification
#' eclass <- with(emdb, assign_promise2015(mmr_ihc_2, pole_mut, p53))
#' esmo2020_w_eclass <- with(emdb, assign_esmo2020(stage_full, grade_rev,
#' hist_rev_gr, hist, myo, lvi, eclass))
#' table(esmo2020_w_eclass)
assign_esmo2020 <- function(stage, grade, hist_gr, hist, myo, lvi,
                            eclass = NULL) {
  # Molecular classification unknown
  if (is.null(eclass)) {
    dplyr::case_when(
      # low
      stage == "IA" &
        grade %in% c("grade 1", "grade 2") &
        hist_gr == "endometrioid" &
        lvi %in% c("negative", "focal") ~ VC.LOW,

      # intermediate
      (stage == "IB" & grade %in% c("grade 1", "grade 2") & hist_gr == "endometrioid" & lvi %in% c("negative", "focal")) |
        (stage == "IA" & grade == "grade 3" & hist_gr == "endometrioid" & lvi %in% c("negative", "focal")) |
        (stage == "IA" & grepl("non-endometrioid|mixed", hist_gr) & hist %in% c("serous", "carcinosarcoma (MMMT)", "undifferentiated") & myo == "none") ~ VC.INTERM,

      # high-intermediate
      (stage %in% c("I", "IA", "IB") & hist_gr == "endometrioid" & lvi %in% c("positive", "extensive") ) |
        (stage == "IB" & grade == "grade 3" & hist_gr == "endometrioid") |
        (stage %in% c("II", "IIA", "IIB") & hist_gr == "endometrioid") ~ VC.HIGH.INTERM,

      # high
      (stage %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2", "IV", "IVA") & hist_gr == "endometrioid") |
        (stage == "IA" & grepl("non-endometrioid|mixed", hist_gr) & hist %in% c("serous", "carcinosarcoma (MMMT)", "undifferentiated") & myo != "none") |
        (stage %in% c("I", "IB", "II", "IIA", "IIB", "III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2", "IV", "IVA") & grepl("non-endometrioid|mixed", hist_gr) & hist %in% c("serous", "carcinosarcoma (MMMT)", "undifferentiated")) ~ VC.HIGH,

      # advanced
      stage %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2", "IV", "IVA") ~ VC.ADVANCED,

      # metastatic
      stage == "IVB" ~ VC.METASTATIC,

      # unassignable
      TRUE ~ NA_character_
    )
  } else {  # molecular classification known
    dplyr::case_when(
      # low
      (grepl("^(I|II)[A-C]?(/C)?$", stage) & eclass == "POLEmut") |
        (stage == "IA" &
           grade %in% c("grade 1", "grade 2") &
           hist_gr == "endometrioid" &
           lvi %in% c("negative", "focal") &
         eclass %in% c("MMRd", "NSMP/p53wt")) ~ VC.LOW,

      # intermediate
      (stage == "IB" & grade %in% c("grade 1", "grade 2") & hist_gr == "endometrioid" & lvi %in% c("negative", "focal") & eclass %in% c("MMRd", "NSMP/p53wt")) |
        (stage == "IA" & grade == "grade 3" & hist_gr == "endometrioid" & lvi %in% c("negative", "focal") & eclass %in% c("MMRd", "NSMP/p53wt")) |
        (stage == "IA" & grepl("non-endometrioid|mixed", hist_gr) & hist %in% c("serous", "carcinosarcoma (MMMT)", "undifferentiated") & myo == "none" & eclass == "p53abn") ~ VC.INTERM,

      # high-intermediate
      (stage %in% c("I", "IA", "IB") & hist_gr == "endometrioid" & lvi %in% c("positive", "extensive") & eclass %in% c("MMRd", "NSMP/p53wt")) |
        (stage == "IB" & grade == "grade 3" & hist_gr == "endometrioid" & eclass %in% c("MMRd", "NSMP/p53wt")) |
        (stage %in% c("II", "IIA", "IIB") & hist_gr == "endometrioid" & eclass %in% c("MMRd", "NSMP/p53wt")) ~ VC.HIGH.INTERM,

      # high
      (stage %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2", "IV", "IVA") & hist_gr == "endometrioid" & eclass %in% c("MMRd", "NSMP/p53wt")) |
        (stage != "IVB" & hist_gr == "endometrioid" & myo != "none" & eclass == "p53abn") |
        (stage == "IA" & grepl("non-endometrioid|mixed", hist_gr) & hist %in% c("serous", "carcinosarcoma (MMMT)", "undifferentiated") & myo != "none"& eclass %in% c("MMRd", "NSMP/p53wt")) |
        (stage %in% c("I", "IB", "II", "IIA", "IIB", "III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2", "IV", "IVA") & grepl("non-endometrioid|mixed", hist_gr) & hist %in% c("serous", "carcinosarcoma (MMMT)", "undifferentiated") & eclass %in% c("MMRd", "NSMP/p53wt")) ~ VC.HIGH,

      # advanced
      stage %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2", "IV", "IVA") ~ VC.ADVANCED,

      # metastatic
      stage == "IVB" ~ VC.METASTATIC,

      # unassignable
      TRUE ~ NA_character_
    )
  }
}
