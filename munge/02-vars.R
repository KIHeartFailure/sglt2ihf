

# Variables for tabs/mods -------------------------------------------------


tabvars <- c(
  # type of sglt2
  "sos_ddr_sglt2_Dapagliflozin",
  "sos_ddr_sglt2_Empagliflozin",
  "sos_ddr_sglt2prevusers",
  
  # demo
  "shf_indexyearquarter",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_location",
  "shf_sos_prevhfh",
  "shf_followuphfunit", "shf_followuplocation_cat",

  # clinical factors and lab measurments
  "shf_ef",
  "shf_durationhf",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmi",
  "shf_bmi_cat",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_hb",
  "shf_ntprobnp",
  "shf_ntprobnp_cat",

  # treatments
  "shf_rasiarni",
  "shf_mra",
  "shf_digoxin",
  "shf_diuretic",
  "shf_nitrate",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_bbl",
  "shf_device_cat",

  # comorbs
  "shf_smoke_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_diabetestype",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_pad",
  "sos_com_stroke",
  "shf_sos_com_af",
  "shf_anemia",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_cancer3y",
  "sos_com_copd",
  "sos_com_dialysis",
  "sos_com_charlsonci_cat",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat"
)

# vars fox log reg and cox reg
tabvars_not_in_mod <- c(
  "sos_ddr_sglt2_Dapagliflozin",
  "sos_ddr_sglt2_Empagliflozin",
  "sos_ddr_sglt2prevusers",
  
  "shf_indexyearquarter",
  
  "shf_age",
  "shf_location",
  "shf_nyha",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_hb",
  "shf_ntprobnp",
  "shf_potassium",
  "shf_bmi",
  "sos_com_dialysis",
  "shf_sos_com_diabetestype",
  "sos_com_charlsonci_cat"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]
modvars <- c(modvars, "shf_indexyearmonth_num")