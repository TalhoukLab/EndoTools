#' Assign MMR status by 2 IHC markers
#'
#' Assign MMR status by 2 IHC markers, MSH6 and PMS2.
#'
#' MMR status, intact or deficient, is determined by IHC markers MSH6 and PMS2
#' consolidated from TMA scoring. If both markers are intact, then MMR status is
#' intact. Otherwise, MMR status is deficient (including subclonal and unknown
#' markers).
#'
#' @param msh6 MSH6 marker
#' @param pms2 PMS2 marker
#' @return MMR status, either "intact" or "deficient"
#' @author Samuel Leung, Derek Chiu
#' @export
assign_mmr2 <- function(msh6, pms2) {
  dplyr::if_else(msh6 == "intact" & pms2 == "intact", "intact", "deficient")
}
