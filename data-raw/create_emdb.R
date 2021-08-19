# Simulated clinical data
library(forcats)
library(tibble)
library(usethis)
n <- 800
set.seed(2021)

emdb <- tibble(
  stage_full = sample(
    x = c("I", "IA", "IB", "IC", "II", "IIA", "IIB", "III", "IIIA", "IIIB",
          "IIIC", "IIIC1", "IIIC2", "IV", "IVA", "IVB"),
    size = n,
    replace = TRUE,
    prob = c(
      0.05, 0.47, 0.14, 0.01, 0.06, 0.01, 0.01, 0.05,
      0.04, 0.01, 0.03, 0.03, 0.01, 0.06, 0.01, 0.01
    )
  ),
  stage = fct_collapse(
    stage_full,
    I = c("I", "IA", "IB", "IC"),
    II = c("II", "IIA", "IIB"),
    III = c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2"),
    IV = c("IV", "IVA", "IVB")
  ),
  grade_rev = sample(
    x = c("grade 1", "grade 2", "grade 3"),
    size = n,
    replace = TRUE,
    prob = c(0.4, 0.15, 0.45)
  ),
  hist_rev_gr = sample(
    x = c("endometrioid", "non-endometrioid"),
    size = n,
    replace = TRUE,
    prob = c(0.7, 0.3)
  ),
  hist = sample(
    x = c(
      "adenocarcinoma, NOS", "carcinosarcoma (MMMT)",
      "clear cell", "dedifferentiated", "endometrioid", "endometrioid (squamous)",
      "endometrioid (villoglandular)", "high grade NOS", "large cell neuroendocrine",
      "mixed endometrioid and clear cell", "mixed endometrioid and mucinous",
      "mixed endometrioid and serous", "mixed endometrioid and undifferentiated",
      "mixed endometrioid, clear cell and mucinous", "mixed serous and carcinosarcoma",
      "mixed serous and clear cell", "mucinous", "other", "serous",
      "small cell", "undifferentiated"
    ),
    size = n,
    replace = TRUE,
    prob = c(
      c(0.003, 0.005, 0.079, 0.003, 0.68, 0.004, 0.003, 0.003, 0.001,
        0.004, 0.001, 0.014, 0.001, 0.001, 0.003, 0.005, 0.001, 0.003,
        0.173, 0.004, 0.009)
    )
  ),
  myo = sample(
    x = c("none", "1-50%", ">50%"),
    size = n,
    replace = TRUE,
    prob = c(0.2, 0.4, 0.4)
  ),
  lvi = sample(
    x = c("negative", "positive"),
    size = n,
    replace = TRUE,
    prob = c(0.7, 0.3)
  ),
  mmr_ihc_2 = sample(
    x = c("intact", "deficient"),
    size = n,
    replace = TRUE,
    prob = c(0.75, 0.25)
  ),
  mmr_ihc_4 = replace(
    x = mmr_ihc_2,
    list = sample(which(mmr_ihc_2 == "intact"), size = 5),
    values = "deficient"
  ),
  pole_mut = sample(
    x = c("wild type", "mutated/non-path", "mutated"),
    size = n,
    replace = TRUE,
    prob = c(0.9, 0.03, 0.07)
  ),
  p53 = sample(
    x = c("wild type", "null", "abnormal", "cytoplasmic", "overexpression"),
    size = n,
    replace = TRUE,
    prob = c(0.8, 0.03, 0.02, 0.01, 0.14)
  ),
  p53_mut = fct_collapse(
    p53,
    mutated = c("null", "abnormal", "cytoplasmic", "overexpression")
  )
)

use_data(emdb)
