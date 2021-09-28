#' Assign ProMisE 2015
#'
#' Assign ProMisE 2015 based on MMR, POLE, and p53.
#'
#' ProMisE 2015 is assigned in the following order:
#' 1. MMR deficient cases if either of the 2 IHCs MSH6 or PMS2 are absent
#' 2. POLE all mutations (pathogenic and non-pathogenic) among MMR intact cases
#' 3. p53 IHC wild type or mutated among POLE wild type cases
#'
#' @param mmr_ihc_2 MMR status determined by 2 IHCs: MSH6 and PMS2
#' @param pole_mut POLE mutation status
#' @param p53 p53 IHC
#' @return ProMisE 2015 assigned into "MMRd", "POLEmut", "p53abn", or "NSMP/p53wt"
#' @author Derek Chiu, Samuel Leung
#' @export
#' @examples
#' promise2015 <- with(emdb, assign_promise2015(mmr_ihc_2, pole_mut, p53))
#' table(promise2015)
assign_promise2015 <- function(mmr_ihc_2, pole_mut, p53) {
  # Validate inputs
  check_input(mmr_ihc_2, MMR_STD)
  check_input(pole_mut, POLE_STD)
  check_input(p53, P53_STD)

  # Assign ProMisE
  factor(
    dplyr::case_when(
      mmr_ihc_2 == "deficient" ~ "MMRd",
      mmr_ihc_2 == "intact" & grepl("mutated", pole_mut) ~ "POLEmut",
      pole_mut == "wild type" &
        grepl("mutated|abnormal|null|overexpression|cytoplasmic", p53) ~ "p53abn",
      pole_mut == "wild type" & p53 == "wild type" ~ "NSMP/p53wt",
      TRUE ~ NA_character_
    ),
    levels = c("MMRd", "POLEmut", "p53abn", "NSMP/p53wt")
  )
}
