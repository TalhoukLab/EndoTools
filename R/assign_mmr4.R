#' Assign MMR status by 4 IHC markers
#'
#' Assign MMR status by 4 IHC markers, MSH6, PMS2, MLH1, and MSH2.
#'
#' MMR status, intact or deficient, is determined by IHC markers MSH6, PMS2,
#' MLH1, and MSH2 consolidated from TMA scoring. If all markers are intact, then
#' MMR status is intact. Otherwise, MMR status is deficient (including subclonal
#' and unknown markers).
#'
#' @inheritParams assign_mmr2
#' @param mlh1 MLH1 marker
#' @param msh2 MSH2 marker
#' @return MMR status, either "intact" or "deficient"
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples
#' mmr_ihc_4 <- with(emdb, assign_mmr4(msh6, pms2, mlh1, msh2))
#' table(mmr_ihc_4)
assign_mmr4 <- function(msh6, pms2, mlh1, msh2) {
  dplyr::if_else(
    msh6 == "intact" &
      pms2 == "intact" &
      mlh1 == "intact" &
      msh2 == "intact",
    "intact",
    "deficient"
  )
}
