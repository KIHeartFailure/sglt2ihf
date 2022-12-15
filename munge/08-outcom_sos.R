

# Comorbidities -----------------------------------------------------------

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "hypertension",
  diakod = " I1[0-5]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "diabetes",
  diakod = " E1[0-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "ihd",
  diakod = " 41[0-4]| I2[0-5]",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  name = "pci",
  opkod = " FNG",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "cabg",
  diakod = " Z951| Z955",
  opkod = " FNA| FNB| FNC| FND| FNE| FNF| FNH",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "pad",
  diakod = " I7[0-3]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "af",
  diakod = " I48",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "stroke",
  diakod = " 43[0-4]| 438| I6[0-4]| I69[0-4]",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "valvular",
  diakod = " I0[5-8]| I3[4-9]| Q22| Q23[0-3]| Q23[0-3]| Q23[5-9]| Z95[2-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "dialysis",
  diakod = " Z491| Z492",
  opkod = " DR014| DR015| DR016| DR020| DR012| DR013| DR023| DR024| TJA33| TJA35",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "copd",
  diakod = " J4[0-4]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "liver",
  diakod = " B18| I85| I864| I982| K70| K710| K711| K71[3-7]| K7[2-4]| K760| K76[2-9]",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "com",
  name = "cancer3y",
  diakod = " C",
  stoptime = -3 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)

outcommeta <- metaout
rm(metaout)

# Time since last HF hospitalization --------------------------------------

hfhospsos <- patreg %>%
  filter(sos_source == "sv") %>%
  mutate(tmp_hfhospsos = stringr::str_detect(HDIA, global_hficd)) %>%
  filter(tmp_hfhospsos)

hfhosp <- inner_join(
  rsdata %>% select(lopnr, shf_indexdtm),
  hfhospsos,
  by = "lopnr"
) %>%
  mutate(tmp_sosdtm = coalesce(UTDATUM, INDATUM)) %>%
  filter(tmp_sosdtm <= shf_indexdtm) %>%
  group_by(lopnr, shf_indexdtm) %>%
  arrange(tmp_sosdtm) %>%
  slice(n()) %>%
  ungroup() %>%
  select(lopnr, shf_indexdtm, tmp_sosdtm)

rsdata <- left_join(
  rsdata,
  hfhosp,
  by = c("lopnr", "shf_indexdtm")
) %>%
  mutate(
    sos_timeprevhosphf = as.numeric(shf_indexdtm - tmp_sosdtm),
    sos_timeprevhosphf = case_when(
      is.na(sos_timeprevhosphf) ~ NA_real_,
      TRUE ~ sos_timeprevhosphf
    )
  ) %>%
  select(-tmp_sosdtm)
