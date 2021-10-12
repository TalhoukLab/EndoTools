#' Simulated endometrial cancer Database
#'
#' Simulated endometrial cancer database used for testing functions.
#'
#' Each clinical variable has a distribution simulated from the proportions seen
#' in the Vancouver endometrial cancer database.
#'
#' * stage_full
#'   * full FIGO staging
#' * stage
#'   * stage without substages: I, II, III, IV
#' * grade_rev
#'   * tumour grade
#' * hist_rev_gr
#'   * histological subtype group
#' * hist
#'   * histological subtype
#' * myo
#'   * myometrial invasion
#' * lvi
#'   * lymphovascular invasion (LVSI)
#' * residual
#'   * residual disease
#' * mmr_ihc_2
#'   * MMR status determined by 2 IHC markers
#' * mmr_ihc_4
#'   * MMR status determined by 4 IHC markers
#' * pole_mut
#'   * POLE mutation status
#' * p53
#'   * p53 IHC
#' * p53_mut
#'   * p53 mutation status
#' * msh6, pms2, mlh1, msh2
#'   * MMR IHC markers
#' @examples
#' emdb
"emdb"
