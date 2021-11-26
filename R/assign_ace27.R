#' Assign ACE-27
#'
#' Assign Adult Comorbidity Evaluation-27 index based on 26 medical
#' comorbidities.
#'
#' Each comorbidity is scored on a scale of Grades 0-3, where 0 = None, 1 =
#' Mild, 2 = Moderate, and 3 = Severe. The ACE-27 overall score is assigned
#' according to the highest ranked individual comorbidity recorded. However, if
#' there are two or more Grade 2 comorbidities occuring in different organ
#' systems, then the overall score becomes Grade 3.
#'
#' Two other scoring assumptions are made. First, if there is insufficient
#' information regarding a comorbidity then it is described as an "NOS" (not
#' otherwise specified) case, and scored as Grade 1. Secondly, any case with
#' multiple symptoms from different Grades is scored into the higher Grade. For
#' example, someone with symptoms from both Grade 1 and Grade 2 is categorized
#' as Grade 2.
#'
#' ## ACE comorbidities by organ systems
#' - Cardiovascular System
#'   - Myocardial Infarction
#'   - Angina / Coronary Heart Disease
#'   - Congestive Heart Failure
#'   - Arrhythmias
#'   - Hypertension
#'   - Venous Disease
#'   - Peripheral Arterial Disease
#' - Respiratory System
#' - Gastrointestinal System
#'   - Hepatic
#'   - Stomach / Intestine
#'   - Pancreas
#' - Renal System
#'   - End-stage renal disease
#' - Endocrine System
#'   - Diabetes Mellitus
#' - Neurological System
#'   - Stroke
#'   - Dementia
#'   - Paralysis
#'   - Neuromuscular
#' - Psychiatric
#' - Rheumatologic
#' - Immunological System
#'   - AIDS
#' - Malignancy
#'   - Solid Tumor including melanoma
#'   - Leukemia and Myeloma
#'   - Lymphoma
#' - Substance Abuse
#'   - Alcohol
#'   - Illicit Drugs
#' - Body Weight
#'   - Obesity
#' @param ace_mi Myocardial Infarction
#' @param ace_cad Angina / Coronary Heart Disease
#' @param ace_chf Congestive Heart Failure
#' @param ace_arr Arrhythmias
#' @param ace_htn Hypertension
#' @param ace_vd Venous Disease
#' @param ace_pad Peripheral Arterial Disease
#' @param ace_res Respiratory System
#' @param ace_hep Hepatic
#' @param ace_sto Stomach / Intestine
#' @param ace_pan Pancreas
#' @param ace_rd End-stage renal disease
#' @param ace_dm Diabetes Mellitus
#' @param ace_str Stroke
#' @param ace_dem Dementia
#' @param ace_par Paralysis
#' @param ace_neu Neuromuscular
#' @param ace_psy Psychiatric
#' @param ace_rhe Rheumatologic
#' @param ace_aid AIDS
#' @param ace_st Solid Tumor including melanoma
#' @param ace_lm Leukemia and Myeloma
#' @param ace_lym Lymphoma
#' @param ace_alc Alcohol
#' @param ace_id Illicit Drugs
#' @param ace_obe Obesity
#' @return A factor with levels "None", "Mild", "Moderate", "Severe" indicating
#'   the overall ACE-27 score.
#' @references Binder PS, Peipert JF, Kallogjeri D, et al. Adult Comorbidity
#'   Evaluation 27 score as a predictor of survival in endometrial cancer
#'   patients. Am J Obstet Gynecol. 2016;215(6):766.e1-766.e9.
#'   doi:10.1016/j.ajog.2016.07.035
#' @references https://siog.org/files/public/ace27.pdf
#' @author Derek Chiu
#' @export
assign_ace27 <- function(ace_mi, ace_cad, ace_chf, ace_arr, ace_htn, ace_vd,
                         ace_pad, ace_res, ace_hep, ace_sto, ace_pan, ace_rd,
                         ace_dm, ace_str, ace_dem, ace_par, ace_neu, ace_psy,
                         ace_rhe, ace_aid, ace_st, ace_lm, ace_lym, ace_alc,
                         ace_id, ace_obe) {

  # Place all variables in data frame
  d <- data.frame(ace_mi, ace_cad, ace_chf, ace_arr, ace_htn, ace_vd,
                  ace_pad, ace_res, ace_hep, ace_sto, ace_pan, ace_rd,
                  ace_dm, ace_str, ace_dem, ace_par, ace_neu, ace_psy,
                  ace_rhe, ace_aid, ace_st, ace_lm, ace_lym, ace_alc,
                  ace_id, ace_obe)

  # Recode into risk scores
  d <- d %>%
    dplyr::mutate(
      ace_mi_score = score_ace(ace_mi, ACE_MI),
      ace_cad_score = score_ace(ace_cad, ACE_CAD),
      ace_chf_score = score_ace(ace_chf, ACE_CHF),
      ace_arr_score = score_ace(ace_arr, ACE_ARR),
      ace_htn_score = score_ace(ace_htn, ACE_HTN),
      ace_vd_score = score_ace(ace_vd, ACE_VD),
      ace_pad_score = score_ace(ace_pad, ACE_PAD),
      ace_res_score = score_ace(ace_res, ACE_RES),
      ace_hep_score = score_ace(ace_hep, ACE_HEP),
      ace_sto_score = score_ace(ace_sto, ACE_STO),
      ace_pan_score = score_ace(ace_pan, ACE_PAN),
      ace_rd_score = score_ace(ace_rd, ACE_RD),
      ace_dm_score = score_ace(ace_dm, ACE_DM),
      ace_str_score = score_ace(ace_str, ACE_STR),
      ace_dem_score = score_ace(ace_dem, ACE_DEM),
      ace_par_score = score_ace(ace_par, ACE_PAR),
      ace_neu_score = score_ace(ace_neu, ACE_NEU),
      ace_psy_score = score_ace(ace_psy, ACE_PSY),
      ace_rhe_score = score_ace(ace_rhe, ACE_RHE),
      ace_aid_score = score_ace(ace_aid, ACE_AID),
      ace_st_score = score_ace(ace_st, ACE_ST),
      ace_lm_score = score_ace(ace_lm, ACE_LM),
      ace_lym_score = score_ace(ace_lym, ACE_LYM),
      ace_alc_score = score_ace(ace_alc, ACE_ALC),
      ace_id_score = score_ace(ace_id, ACE_ID),
      ace_obe_score = score_ace(ace_obe, ACE_OBE)
    )

  # Aggregate max scores within each body system
  d <- d %>%
    dplyr::mutate(
      ace_sys_car = purrr::pmap_dbl(dplyr::select(
        .,
        c(
          "ace_mi_score",
          "ace_cad_score",
          "ace_chf_score",
          "ace_arr_score",
          "ace_htn_score",
          "ace_vd_score",
          "ace_pad_score"
        )
      ), pmax, na.rm = TRUE),
      ace_sys_res = .data$ace_res_score,
      ace_sys_gi = purrr::pmap_dbl(dplyr::select(., c(
        "ace_hep_score", "ace_sto_score", "ace_pan_score"
      )), pmax, na.rm = TRUE),
      ace_sys_ren = .data$ace_rd_score,
      ace_sys_end = .data$ace_dm_score,
      ace_sys_neu = purrr::pmap_dbl(dplyr::select(
        .,
        c(
          "ace_str_score",
          "ace_dem_score",
          "ace_par_score",
          "ace_neu_score"
        )
      ), pmax, na.rm = TRUE),
      ace_sys_psy = .data$ace_psy_score,
      ace_sys_rhe = .data$ace_rhe_score,
      ace_sys_imm = .data$ace_aid_score,
      ace_sys_mal = purrr::pmap_dbl(dplyr::select(., c(
        "ace_st_score", "ace_lm_score", "ace_lym_score"
      )), pmax, na.rm = TRUE),
      ace_sys_sub = purrr::pmap_dbl(dplyr::select(., c("ace_alc_score", "ace_id_score")), pmax, na.rm = TRUE),
      ace_sys_bod = .data$ace_obe_score
    )

  # Assign ACE-27, adjusting for multiple systems with moderate risk
  d <- d %>%
    dplyr::mutate(
      ace_multi_sys = purrr::pmap_lgl(
        dplyr::select(., dplyr::matches("ace_sys")),
        purrr::lift_vd(function(x) sum(x == 2, na.rm = TRUE) > 1)
      ),
      ace_27 = purrr::pmap_dbl(dplyr::select(., dplyr::matches("_score")), pmax, na.rm = TRUE) %>%
        ifelse(.data$ace_multi_sys, 3, .) %>%
        factor(labels = c("None", "Mild", "Moderate", "Severe"))
    )

  d[["ace_27"]]
}

#' Score ACE variables based on highest grade from standard symptoms list
#' @noRd
score_ace <- function(ace, symptoms) {
  symptoms %>%
    purrr::map(~ gsub("\\(", "\\\\(", .) %>%
                 gsub("\\)", "\\\\)", .) %>%
                 paste(collapse = "|")) %>%
    rlang::list2(!!!., `0` = "none") %>%
    purrr::imap(~ ifelse(grepl(.x, ace), as.numeric(.y), NA_real_)) %>%
    purrr::pmap_dbl(pmax, na.rm = TRUE)
}
