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
      ace_mi_score = dplyr::case_when(
        ace_mi == "none" ~ 0,
        ace_mi %in% c("ECG only, age unk", "Myocardial Infarct, NOS") ~ 1,
        ace_mi == "> 6 months" ~ 2,
        TRUE ~ NA_real_
      ),
      ace_cad_score = dplyr::case_when(
        ace_cad == "none" ~ 0,
        ace_cad %in% c(
          "CABG or PTCA (> 6 months)",
          "Coronary stent (> 6 months)",
          "CABG or PTCA (> 6 months)+Coronary stent (> 6 months)",
          "Angina / Coronary Artery Disease, NOS"
        ) ~ 1,
        ace_cad %in% c(
          "Chronic exertional angina",
          "Chronic exertional angina+Angina / Coronary Artery Disease, NOS",
          "Recent (<= 6 months) coronary stent+none",
          "Recent (<= 6 months) Coronary Artery Bypass Graft (CABG) or Percutaneous Transluminal Coronary Angioplasty (PTCA)+Angina / Coronary Artery Disease, NOS"
        ) ~ 2,
        TRUE ~ NA_real_
      ),
      ace_chf_score = dplyr::case_when(
        ace_chf == "none" ~ 0,
        ace_chf %in% c("CHF with dyspnea which has responded to treatment", "CHF, NOS") ~ 1,
        TRUE ~ NA_real_
      ),
      ace_arr_score = dplyr::case_when(
        ace_arr == "none" ~ 0,
        ace_arr == "Supraventricular tachycardia" ~ 1,
        ace_arr %in% c(
          "Chronic atrial fibrillation or flutter",
          "Chronic atrial fibrillation or flutter+Sick Sinus Syndrome",
          "Chronic atrial fibrillation or flutter+Pacemaker+Sick Sinus Syndrome",
          "Pacemaker",
          "Arrhythmias, NOS"
        ) ~ 2,
        TRUE ~ NA_real_
      ),
      ace_htn_score = dplyr::case_when(
        ace_htn == "none" ~ 0,
        ace_htn %in% c(
          "DBP 90-114 mm Hg w/o antihypertensive meds",
          "DBP 90-114 mm Hg w/o antihypertensive meds+Hypertension, NOS",
          "DBP < 90 mm Hg w/ antihypertensive meds",
          "Hypertension, NOS"
        ) ~ 1,
        ace_htn %in% c("DBP 90-114 mm Hg w/ antihypertensive meds") ~ 2,
        ace_htn %in% c("DBP > 130 mm Hg") ~ 3,
        TRUE ~ NA_real_
      ),
      ace_vd_score = dplyr::case_when(
        ace_vd == "none" ~ 0,
        ace_vd %in% c(
          "Old DVT no longer treated with Coumadin or Heparin",
          "Venous Disease, NOS"
        ) ~ 1,
        ace_vd %in% c(
          "DVT controlled with Coumadin or heparin",
          "DVT controlled with Coumadin or heparin+Old PE > 6 months",
          "DVT controlled with Coumadin or heparin+Venous Disease, NOS",
          "Old PE > 6 months",
          "Old PE > 6 months+Old DVT no longer treated with Coumadin or Heparin"
        ) ~ 2,
        ace_vd %in% c(
          "Recent PE (<= 6 months)",
          "Recent PE (<= 6 months)+Use of venous filter for PEs",
          "Recent PE (<= 6 months)+Use of venous filter for PEs+DVT controlled with Coumadin or heparin",
          "Recent PE (<= 6 months)+DVT controlled with Coumadin or heparin",
          "Use of venous filter for PEs+DVT controlled with Coumadin or heparin"
        ) ~ 3,
        TRUE ~ NA_real_
      ),
      ace_pad_score = dplyr::case_when(
        ace_pad == "none" ~ 0,
        ace_pad %in% c("s/p abdominal or thoracic aortic aneurysm repair", "Peripheral Arterial Disease, NOS") ~ 1,
        ace_pad == "Bypass or amputation for gangrene or arterial insufficiency > 6 months ago" ~ 2,
        TRUE ~ NA_real_
      ),
      ace_res_score = dplyr::case_when(
        ace_res == "none" ~ 0,
        ace_res %in% c("Restrictive Lung Disease or COPD (chronic bronchitis, emphysema, or asthma) with dyspnea which has responded to treatment",
                       "Respiratory system disease, NOS") ~ 1,
        ace_res == "Restrictive Lung Disease or COPD (chronic bronchitis, emphysema, or asthma) with dyspnea which limits activities" ~ 2,
        TRUE ~ NA_real_
      ),
      ace_hep_score = dplyr::case_when(
        ace_hep == "none" ~ 0,
        ace_hep %in% c("Chronic hepatitis or cirrhosis without portal hypertension+Hepatic disease, NOS",
                       "Hepatic disease, NOS") ~ 1,
        TRUE ~ NA_real_
      ),
      ace_sto_score = dplyr::case_when(
        ace_sto == "none" ~ 0,
        ace_sto %in% c(
          "Diagnosis of ulcers treated with meds",
          "Inflammatory bowel disease (IBD) on meds or h/o with complications and/or surgery",
          "Stomach / Intestine disease, NOS"
        ) ~ 1,
        TRUE ~ NA_real_
      ),
      ace_pan_score = dplyr::case_when(
        ace_pan == "none" ~ 0,
        ace_pan == "Pancreatic disease, NOS" ~ 1,
        ace_pan == "Chronic pancreatitis with minor complications (malabsorption, impaired glucose tolerance, or GI bleeding)" ~ 2,
        TRUE ~ NA_real_
      ),
      ace_rd_score = dplyr::case_when(
        ace_rd == "none" ~ 0,
        ace_rd %in% c("Chronic Renal Insufficiency with creatinine 2-3 mg%",
                      "End-stage renal disease, NOS") ~ 1,
        ace_rd == "Chronic dialysis" ~ 2,
        ace_rd == "Acute dialysis" ~ 3,
        TRUE ~ NA_real_
      ),
      ace_dm_score = dplyr::case_when(
        ace_dm == "none" ~ 0,
        ace_dm %in% c("AODM controlled by oral agents only",
                      "Diabetes Mellitus, NOS") ~ 1,
        ace_dm %in% c(
          "IDDM without complications",
          "Poorly controlled AODM with oral agents"
        ) ~ 2,
        ace_dm %in% c(
          "retinopathy",
          "retinopathy+nephropathy+AODM controlled by oral agents only",
          "retinopathy+Diabetes Mellitus, NOS",
          "neuropathy",
          "neuropathy+AODM controlled by oral agents only",
          "neuropathy+Diabetes Mellitus, NOS",
          "nephropathy+Diabetes Mellitus, NOS"
        ) ~ 3,
        TRUE ~ NA_real_
      ),
      ace_str_score = dplyr::case_when(
        ace_str == "none" ~ 0,
        ace_str %in% c("Past or recent TIA", "Stroke, NOS") ~ 1,
        ace_str == "Old stroke with neurologic residual" ~ 2,
        TRUE ~ NA_real_
      ),
      ace_dem_score = dplyr::case_when(
        ace_dem == "none" ~ 0,
        ace_dem %in% c("Mild dementia (can take care of self)", "Dementia, NOS") ~ 1,
        TRUE ~ NA_real_
      ),
      ace_par_score = dplyr::case_when(
        ace_par == "none" ~ 0,
        ace_par == "Paralysis, NOS" ~ 1,
        TRUE ~ NA_real_
      ),
      ace_neu_score = dplyr::case_when(
        ace_neu == "none" ~ 0,
        ace_neu %in% c(
          "MS, Parkinson's, Myasthenia Gravis, or other chronic neuromuscular disorder, but ambulatory and providing most of self care",
          "Neuromuscular disorders, NOS"
        ) ~ 1,
        ace_neu == "MS, Parkinson's, Myasthenia Gravis, or other chronic neuromuscular disorder, but able to do some self care" ~ 2,
        TRUE ~ NA_real_
      ),
      ace_psy_score = dplyr::case_when(
        ace_psy == "none" ~ 0,
        ace_psy %in% c(
          "Depression or bipolar disorder controlled w/ medication",
          "Depression or bipolar disorder controlled w/ medication+Psychiatric disorders, NOS",
          "Psychiatric disorders, NOS"
        ) ~ 1,
        ace_psy %in% c(
          "Depression or bipolar disorder uncontrolled",
          "Schizophrenia controlled w/ meds",
          "Schizophrenia controlled w/ meds+Psychiatric disorders, NOS"
        ) ~ 2,
        ace_psy == "Active schizophrenia" ~ 3,
        TRUE ~ NA_real_
      ),
      ace_rhe_score = dplyr::case_when(
        ace_rhe == "none" ~ 0,
        ace_rhe %in% c(
          "Connective Tissue Disorder on NSAIDS or no treatment",
          "Connective Tissue Disorder on NSAIDS or no treatment+Rheumatologic, NOS",
          "Rheumatologic, NOS"
        ) ~ 1,
        ace_rhe == "Connective Tissue Disorder on steroids or immunosuppressant medications" ~ 2,
        TRUE ~ NA_real_
      ),
      ace_aid_score = dplyr::case_when(
        ace_aid == "none" ~ 0,
        TRUE ~ NA_real_
      ),
      ace_st_score = dplyr::case_when(
        ace_st == "none" ~ 0,
        ace_st %in% c("Any controlled solid tumor without documented metastases, but initially diagnosed and treated > 5 years ago",
                      "Solid Tumor, NOS") ~ 1,
        ace_st %in% c("Any controlled solid tumor without documented metastases, but initially diagnosed and treated within the last 5 years",
                      "Any controlled solid tumor without documented metastases, but initially diagnosed and treated within the last 5 years+Any controlled solid tumor without documented metastases, but initially diagnosed and treated > 5 years ago") ~ 2,
        ace_st %in% c("Newly diagnosed but not yet treated",
                      "Newly diagnosed but not yet treated+Any controlled solid tumor without documented metastases, but initially diagnosed and treated within the last 5 years",
                      "Metastatic solid tumor") ~ 3,
        TRUE ~ NA_real_
      ),
      ace_lm_score = dplyr::case_when(
        ace_lm == "none" ~ 0,
        TRUE ~ NA_real_
      ),
      ace_lym_score = dplyr::case_when(
        ace_lym == "none" ~ 0,
        ace_lym %in% c("H/o lymphoma w/ last Rx >1 yr prior",
                       "Lymphoma, NOS") ~ 1,
        ace_lym == "1st remission or new dx < 1yr" ~ 2,
        TRUE ~ NA_real_
      ),
      ace_alc_score = dplyr::case_when(
        ace_alc == "none" ~ 0,
        ace_alc %in% c("H/o alcohol abuse but not presently drinking",
                       "Alcohol abuse, NOS") ~ 1,
        TRUE ~ NA_real_
      ),
      ace_id_score = dplyr::case_when(
        ace_id == "none" ~ 0,
        ace_id == "H/o substance abuse but not presently using" ~ 1,
        TRUE ~ NA_real_
      ),
      ace_obe_score = dplyr::case_when(
        ace_obe == "none" ~ 0,
        ace_obe %in% c("Morbid (i.e., BMI >= 38)", "Obesity, NOS") ~ 2,
        TRUE ~ NA_real_
      )
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
