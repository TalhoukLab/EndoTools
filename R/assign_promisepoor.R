#' Assign Poor-man's ProMisE
#'
#' Assign Poor-man's ProMisE based on POLE, MMR, p53, and histopathological
#' features.
#'
#' Poor-man's ProMisE first assigns a low risk group based on histopathological
#' features grade, histotype, MMR status determined by 2 and 4 IHC markers, and
#' p53. The rest of the high risk cases are assigned as in ProMisE 2019.
#'
#' @param mmr_ihc_4 MMR status determined by 4 IHCs: MSH6, PMS2, MLH1, and MSH2
#' @inheritParams assign_promise2019
#' @inheritParams assign_esmo2020
#' @return Poor-man's ProMisE assigned into "Low-Risk", "POLEmut", "MMRd",
#'   "p53abn", or "NSMP/p53wt"
#' @author Derek Chiu, Samuel Leung
#' @export
#' @examples
#' promisepoor <- with(emdb, assign_promisepoor(mmr_ihc_2, mmr_ihc_4, pole_mut,
#' p53, grade_rev, hist_rev_gr))
#' table(promisepoor)
assign_promisepoor <- function(mmr_ihc_2, mmr_ihc_4, pole_mut, p53, grade, hist_gr) {
  factor(
    dplyr::case_when(
      grade %in% c("grade 1", "grade 2") &
        hist_gr == "endometrioid" &
        mmr_ihc_2 == "intact" &
        (is.na(mmr_ihc_4) | mmr_ihc_4 == "intact") &
        p53 == "wild type" ~ "Low-Risk",
      pole_mut == "mutated" ~ "POLEmut",
      pole_mut %in% c("mutated/non-path", "wild type") & mmr_ihc_2 == "deficient" ~ "MMRd",
      mmr_ihc_2 == "intact" & grepl("mutated|abnormal|null|overexpression|cytoplasmic", p53) ~ "p53abn",
      mmr_ihc_2 == "intact" & p53 == "wild type" ~ "NSMP/p53wt",
      TRUE ~ NA_character_
    ),
    levels = c("Low-Risk", "POLEmut", "MMRd", "p53abn", "NSMP/p53wt")
  )
}
