#' Assign ProMisE 2015
#'
#' Assign ProMisE 2015 based on MMR, POLE, and p53.
#'
#' ProMisE 2015 is assigned in the following order:
#' 1. MMR deficient cases if either of the 2 IHCs MSH6 or PMS2 are absent
#' 2. POLE all mutations (pathogenic and non-pathogenic) among MMR intact cases
#' 3. p53 IHC wild type or mutated among POLE wild type cases
#'
#' @param mmr MMR status determined by 2 IHCs: MSH6 and PMS2
#' @param pole POLE mutation status
#' @param p53 p53 IHC
#' @return ProMisE 2015 assigned into "MMRd", "POLEmut", "p53abn", or "NSMP/p53wt"
#' @author Derek Chiu, Samuel Leung
#' @export
assign_promise2015 <- function(mmr, pole, p53) {
  factor(
    dplyr::case_when(
      mmr == "deficient" ~ "MMRd",
      mmr == "intact" & grepl("mutated", pole) ~ "POLEmut",
      pole == "wild type" &
        grepl("mutated|abnormal|null|overexpression|cytoplasmic", p53) ~ "p53abn",
      pole == "wild type" & p53 == "wild type" ~ "NSMP/p53wt",
      TRUE ~ NA_character_
    ),
    levels = c("MMRd", "POLEmut", "p53abn", "NSMP/p53wt")
  )
}
