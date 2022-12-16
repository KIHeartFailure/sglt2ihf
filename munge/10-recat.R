
# ntprobnp
mednt <- median(rsdata$shf_ntprobnp, na.rm = T)

# income
medinc <- median(rsdata$scb_dispincome, na.rm = T)

rsdata <- rsdata %>%
  mutate(
    shf_sos_com_af = ynfac(case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ 1,
      TRUE ~ 0
    )),
    shf_sos_com_ihd = ynfac(case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pci == "Yes" |
        sos_com_cabg == "Yes" ~ 1,
      TRUE ~ 0
    )),
    shf_sos_com_hypertension = ynfac(case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ 1,
      TRUE ~ 0
    )),
    shf_sos_com_diabetes = ynfac(case_when(
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ 1,
      TRUE ~ 0
    )),
    shf_sos_com_diabetestype = case_when(
      (is.na(shf_diabetestype) |
        shf_diabetestype == "No") & shf_sos_com_diabetes == "Yes" ~ NA_real_,
      shf_sos_com_diabetes == "No" ~ 1,
      shf_diabetestype == "Type I" ~ 2,
      shf_diabetestype == "Type II" ~ 3
    ),
    shf_sos_com_diabetestype_mod = if_else(
      shf_sos_com_diabetestype == 2, NA_real_, shf_sos_com_diabetestype
    ),
    shf_sos_com_diabetestype = factor(shf_sos_com_diabetestype,
      levels = 1:3, labels = c("No", "Type I", "Type II")
    ),
    shf_sos_com_diabetestype_mod = factor(shf_sos_com_diabetestype_mod,
      levels = c(1, 3), labels = c("No", "Type II")
    ),
    shf_sos_com_valvular = ynfac(case_when(
      shf_valvedisease == "Yes" |
        sos_com_valvular == "Yes" ~ 1,
      TRUE ~ 0
    )),
    shf_ntprobnp_cat = factor(case_when(
      shf_ntprobnp < mednt ~ 1,
      shf_ntprobnp >= mednt ~ 2
    ),
    levels = 1:2,
    labels = c("<median", ">=median")
    ),
    scb_dispincome_cat = factor(case_when(
      scb_dispincome < medinc ~ 1,
      scb_dispincome >= medinc ~ 2
    ),
    levels = 1:2,
    labels = c("<median", ">=median")
    ),
    shf_indexyearmonth = factor(zoo::as.yearmon(shf_indexdtm)),
    shf_indexyearmonth_num = as.numeric(shf_indexyearmonth),
    shf_quarter = quarter(shf_indexdtm),
    shf_indexyearquarter = paste0(shf_indexyear, ":", shf_quarter),
    sos_com_charlsonci_cat = factor(case_when(
      sos_com_charlsonci <= 1 ~ 1,
      sos_com_charlsonci <= 3 ~ 2,
      sos_com_charlsonci <= 7 ~ 3,
      sos_com_charlsonci >= 8 ~ 4
    ),
    levels = 1:4,
    labels = c(
      "0-1",
      "2-3",
      "4-7",
      ">=8"
    )
    ),
    shf_sos_prevhfh = factor(case_when(
      shf_location == "In-patient" |
        !is.na(sos_timeprevhosphf) & sos_timeprevhosphf < 365 ~ 1,
      TRUE ~ 0
    ),
    levels = 0:1, labels = c(
      "No previous HFH <1 year",
      "Previous HFH <1 year"
    )
    ),
    shf_qol_cat = factor(case_when(
      shf_qol <= 25 ~ 1,
      shf_qol <= 50 ~ 2,
      shf_qol <= 75 ~ 3,
      shf_qol <= 100 ~ 4,
    ),
    levels = 1:4,
    labels = c("0-25", "26-50", "51-75", "76-100")
    )
  )

rsdata <- rsdata %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(
    shf_centre = as.character(shf_centre),
    shf_centreregion = as.character(shf_centreregion),
    sos_deathcause = as.character(sos_deathcause),
    scb_region = as.character(scb_region)
  )
