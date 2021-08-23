#' Assign ProMisE 2019
#'
#' Assign ProMisE 2015 based on POLE, MMR, and p53.
#'
#' ProMisE 2015 is assigned in the following order:
#' 1. POLE pathogenic mutations
#' 2. MMR deficient cases if either of the 2 IHCs MSH6 or PMS2 are absent,
#' among POLE non-pathogenic mutations or wild type cases
#' 3. p53 IHC wild type or mutated among MMR intact cases
#'
#' @inheritParams assign_promise2015
#' @return ProMisE 2019 assigned into "POLEmut", "MMRd", "p53abn", or "NSMP/p53wt"
#' @author Derek Chiu, Samuel Leung
#' @export
#' @examples
#' promise2019 <- with(emdb, assign_promise2019(mmr_ihc_2, pole_mut, p53))
#' table(promise2019)
assign_promise2019 <- function(mmr, pole, p53) {
  factor(
    dplyr::case_when(
      pole == "mutated" ~ "POLEmut",
      pole %in% c("mutated/non-path", "wild type") & mmr == "deficient" ~ "MMRd",
      mmr == "intact" & grepl("mutated|abnormal|null|overexpression|cytoplasmic", p53) ~ "p53abn",
      mmr == "intact" & p53 == "wild type" ~ "NSMP/p53wt",
      TRUE ~ NA_character_
    ),
    levels = c("POLEmut", "MMRd", "p53abn", "NSMP/p53wt")
  )
}
