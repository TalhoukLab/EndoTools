VC.NONE <- "none"
VC.NEGATIVE <- "negative"
VC.POSITIVE <- "positive"

VC.LOW <- "low"
VC.INTERM <- "intermediate"
VC.HIGH.INTERM <- "high-intermediate"
VC.HIGH <- "high"
VC.ADVANCED <- "advanced"
VC.METASTATIC <- "metastatic"

STAGE_STD <- c(
  "I",
  "IA",
  "IB",
  "IC",
  "II",
  "IIA",
  "IIB",
  "III",
  "IIIA",
  "IIIB",
  "IIIC",
  "IIIC1",
  "IIIC2",
  "IV",
  "IVA",
  "IVB"
)

GRADE_STD <- c(
  "grade 1",
  "grade 2",
  "grade 3"
)

HIST_STD <- c(
  "endometrioid",
  "non-endometrioid",
  "non-endometrioid or mixed"
)

MYO_STD <- c(
  "none",
  "1-50%",
  ">50%"
)

LVI_STD <- c(
  "negative",
  "positive",
  "focal",
  "extensive"
)

ECLASS_STD <- c(
  "MMRd",
  "NSMP/p53wt",
  "p53abn",
  "POLEmut"
)

RESIDUAL_STD <- c(
  "no residual",
  "microscopic",
  "<1cm",
  ">=1cm"
)

IHC_MARKER_STD <- c(
  "loss",
  "intact"
)

MMR_STD <- c(
  "deficient",
  "intact"
)

POLE_STD <- c(
  "wild type",
  "mutated",
  "mutated/non-path"
)

P53_STD <- c(
  "wild type",
  "wild type (subclonal)",
  "abnormal",
  "abnormal/NOS",
  "abnormal/NOS (subclonal)",
  "null",
  "null (subclonal)",
  "overexpression",
  "overexpression (subclonal)",
  "cytoplasmic",
  "mutated"
)

ACE_MI <- list(
  `3` = "<= 6 months",
  `2` = "> 6 months",
  `1` = c("ECG only, age unk", "Myocardial Infarct, NOS")
)

ACE_CAD = list(
  `3` = "Unstable angina",
  `2` = c(
    "Chronic exertional angina",
    "Recent (<= 6 months) Coronary Artery Bypass Graft (CABG) or Percutaneous Transluminal Coronary Angioplasty (PTCA)",
    "Recent (<= 6 months) coronary stent"
  ),
  `1` = c(
    "ECG or stress test evidence or catheterization evidence of coronary disease without symptoms",
    "Angina pectoris not requiring hospitalization",
    "CABG or PTCA (> 6 months)",
    "Coronary stent (> 6 months)",
    "Angina / Coronary Artery Disease, NOS"
  )
)

ACE_CHF = list(
  `3` = c(
    "Hospitalized for CHF within past 6 months",
    "Ejection fraction < 20%"
  ),
  `2` = c(
    "Hospitalized for CHF > 6 months prior",
    "CHF with dyspnea which limits activities"
  ),
  `1` = c(
    "CHF with dyspnea which has responded to treatment",
    "Exertional dyspnea",
    "Paroxysmal Nocturnal Dyspnea (PND)",
    "CHF, NOS"
  )
)

ACE_ARR = list(
  `3` = "Ventricular arrhythmia <= 6 months",
  `2` = c(
    "Ventricular arrhythmia > 6 months",
    "Chronic atrial fibrillation or flutter",
    "Pacemaker"
  ),
  `1` = c(
    "Sick Sinus Syndrome",
    "Supraventricular tachycardia",
    "Arrhythmias, NOS"
  )
)

ACE_HTN = list(
  `3` = c(
    "DBP > 130 mm Hg",
    "Severe malignant papilledema or other eye changes",
    "Encephalopathy"
  ),
  `2` = c(
    "DBP 115-129 mm Hg",
    "DBP 90-114 mm Hg w/ antihypertensive meds",
    "2nd cardiovascular symptoms"
  ),
  `1` = c(
    "DBP 90-114 mm Hg w/o antihypertensive meds",
    "DBP < 90 mm Hg w/ antihypertensive meds",
    "Hypertension, NOS"
  )
)

ACE_VD = list(
  `3` = c("Recent PE (<= 6 months)",
          "Use of venous filter for PEs"),
  `2` = c("DVT controlled with Coumadin or heparin",
          "Old PE > 6 months"),
  `1` = c(
    "Old DVT no longer treated with Coumadin or Heparin",
    "Venous Disease, NOS"
  )
)

ACE_PAD = list(
  `3` = c(
    "Bypass or amputation for gangrene or arterial insufficiency < 6 months ago",
    "Untreated thoracic or abdominal aneurysm (> 6 cm)"
  ),
  `2` = c(
    "Bypass or amputation for gangrene or arterial insufficiency > 6 months ago",
    "Chronic insufficiency"
  ),
  `1` = c(
    "Intermittent claudication",
    "Untreated thoracic or abdominal aneurysm (< 6 cm)",
    "s/p abdominal or thoracic aortic aneurysm repair",
    "Peripheral Arterial Disease, NOS"
  )
)

ACE_RES = list(
  `3` = c(
    "Marked pulmonary insufficiency",
    "Restrictive Lung Disease or COPD with dyspnea at rest despite treatment",
    "Chronic supplemental O2",
    "CO2 retention (pCO2 > 50 torr)",
    "Baseline pO2 < 50 torr",
    "FEV1 (< 50%)"
  ),
  `2` = c(
    "Restrictive Lung Disease or COPD (chronic bronchitis, emphysema, or asthma) with dyspnea which limits activities",
    "FEV1 (51%-65%)"
  ),
  `1` = c(
    "Restrictive Lung Disease or COPD (chronic bronchitis, emphysema, or asthma) with dyspnea which has responded to treatment",
    "FEV1 (66%-80%)",
    "Respiratory system disease, NOS"
  )
)

ACE_HEP = list(
  `3` = "Portal hypertension and/or esophageal bleeding <= 6 mos. (Encephalopathy, Ascites, Jaundice with Total Bilirubin > 2)",
  `2` = "Chronic hepatitis, cirrhosis, portal hypertension with moderate symptoms \"compensated hepatic failure\"",
  `1` = c(
    "Chronic hepatitis or cirrhosis without portal hypertension",
    "Acute hepatitis without cirrhosis",
    "Chronic liver disease manifested on biopsy or persistently elevated bilirubin (>3 mg/dl)",
    "Hepatic disease, NOS"
  )
)

ACE_STO = list(
  `3` = "Recent ulcers (<= 6 months ago) requiring blood transfusion",
  `2` = "Ulcers requiring surgery or transfusion > 6 months ago",
  `1` = c(
    "Diagnosis of ulcers treated with meds",
    "Chronic malabsorption syndrome",
    "Inflammatory bowel disease (IBD) on meds or h/o with complications and/or surgery",
    "Stomach / Intestine disease, NOS"
  )
)

ACE_PAN = list(
  `3` = "Acute or chronic pancreatitis with major complications (phlegmon, abscess, or pseudocyst)",
  `2` = c(
    "Uncomplicated acute pancreatitis",
    "Chronic pancreatitis with minor complications (malabsorption, impaired glucose tolerance, or GI bleeding)"
  ),
  `1` = c(
    "Chronic pancreatitis w/o complications",
    "Pancreatic disease, NOS"
  )
)

ACE_RD = list(
  `3` = c(
    "Creatinine > 3 mg% with multi-organ failure, shock, or sepsis",
    "Acute dialysis"
  ),
  `2` = c(
    "Chronic Renal Insufficiency with creatinine >3 mg%",
    "Chronic dialysis"
  ),
  `1` = c(
    "Chronic Renal Insufficiency with creatinine 2-3 mg%",
    "End-stage renal disease, NOS"
  )
)

ACE_DM = list(
  `3` = c(
    "Hospitalization <= 6 months for DKA",
    "retinopathy",
    "neuropathy",
    "nephropathy",
    "coronary disease",
    "peripheral arterial disease"
  ),
  `2` = c(
    "IDDM without complications",
    "Poorly controlled AODM with oral agents"
  ),
  `1` = c("AODM controlled by oral agents only",
          "Diabetes Mellitus, NOS")
)

ACE_STR = list(
  `3` = "Acute stroke with significant neurologic deficit",
  `2` = "Old stroke with neurologic residual",
  `1` = c("Stroke with no residual",
          "Past or recent TIA",
          "Stroke, NOS")
)

ACE_DEM = list(
  `3` = "Severe dementia requiring full support for activities of daily living",
  `2` = "Moderate dementia (not completely self-sufficient, needs supervising)",
  `1` = c("Mild dementia (can take care of self)",
          "Dementia, NOS")
)

ACE_PAR = list(
  `3` = "Paraplegia or hemiplegia requiring full support for activities of daily living",
  `2` = "Paraplegia or hemiplegia requiring wheelchair, able to do some self care",
  `1` = c(
    "Paraplegia or hemiplegia, ambulatory and providing most of self care",
    "Paralysis, NOS"
  )
)

ACE_NEU = list(
  `3` = "MS, Parkinson's, Myasthenia Gravis, or other chronic neuromuscular disorder and requiring full support for activities of daily living",
  `2` = "MS, Parkinson's, Myasthenia Gravis, or other chronic neuromuscular disorder, but able to do some self care",
  `1` = c(
    "MS, Parkinson's, Myasthenia Gravis, or other chronic neuromuscular disorder, but ambulatory and providing most of self care",
    "Neuromuscular disorders, NOS"
  )
)

ACE_PSY = list(
  `3` = c("Recent suicidal attempt",
          "Active schizophrenia"),
  `2` = c(
    "Depression or bipolar disorder uncontrolled",
    "Schizophrenia controlled w/ meds"
  ),
  `1` = c(
    "Depression or bipolar disorder controlled w/ medication",
    "Psychiatric disorders, NOS"
  )
)

ACE_RHE = list(
  `3` = "Connective Tissue Disorder with secondary end-organ failure (renal, cardiac, CNS)",
  `2` = "Connective Tissue Disorder on steroids or immunosuppressant medications",
  `1` = c(
    "Connective Tissue Disorder on NSAIDS or no treatment",
    "Rheumatologic, NOS"
  )
)

ACE_AID = list(
  `3` = "Fulminant AIDS w/KS, MAI, PCP (AIDS defining illness)",
  `2` = "HIV+ with h/o defining illness. CD4+ < 200/uL",
  `1` = c(
    "Asymptomatic HIV+ patient.",
    "HIV+ w/o h/o AIDS defining illness. CD4+ > 200/uL",
    "AIDS, NOS"
  )
)

ACE_ST = list(
  `3` = c(
    "Uncontrolled cancer",
    "Newly diagnosed but not yet treated",
    "Metastatic solid tumor"
  ),
  `2` = "Any controlled solid tumor without documented metastases, but initially diagnosed and treated within the last 5 years",
  `1` = c(
    "Any controlled solid tumor without documented metastases, but initially diagnosed and treated > 5 years ago",
    "Solid Tumor, NOS"
  )
)

ACE_LM = list(
  `3` = c("Relapse",
          "Disease out of control"),
  `2` = c("1st remission or new dx < 1yr",
          "Chronic suppressive therapy"),
  `1` = c(
    "H/o leukemia or myeloma with last Rx > 1 yr prior",
    "Leukemia and Myeloma, NOS"
  )
)

ACE_LYM = list(
  `3` = "Relapse",
  `2` = c("1st remission or new dx < 1yr",
          "Chronic suppressive therapy"),
  `1` = c("H/o lymphoma w/ last Rx >1 yr prior",
          "Lymphoma, NOS")
)

ACE_ALC = list(
  `3` = "Delirium tremens",
  `2` = "Active alcohol abuse with social, behavioral, or medical complications",
  `1` = c(
    "H/o alcohol abuse but not presently drinking",
    "Alcohol abuse, NOS"
  )
)

ACE_ID = list(
  `3` = "Acute Withdrawal Syndrome",
  `2` = "Active substance abuse with social, behavioral, or medical complications",
  `1` = c("H/o substance abuse but not presently using",
          "Illicit drugs, NOS")
)

ACE_OBE = list(`2` = c("Morbid (i.e., BMI >= 38)",
                       "Obesity, NOS"))
