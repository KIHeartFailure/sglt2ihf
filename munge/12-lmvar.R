
# Treatments from DDR ----------------------------------------

# within 5 months prior to index

lmtmp <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm),
  lmsglt2,
  by = c("lopnr" = "LopNr")
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= -30.5 * 5, diff <= 14) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rsdata <- create_medvar(
  atc = global_sglt2atc,
  medname = "sglt2", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-5mo-14days",
  valsclass = "fac"
)
rsdata <- create_medvar(
  atc = "^(A10BK01|A10BD15|A10BD21|A10BD25)",
  medname = "sglt2_Dapagliflozin", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-5mo-14days",
  valsclass = "fac"
)
rsdata <- create_medvar(
  atc = "^(A10BK03|A10BD19|A10BD20)",
  medname = "sglt2_Empagliflozin", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-5mo-14days",
  valsclass = "fac"
)

rsdata <- create_medvar(
  atc = "^(A10BK02|A10BD16)",
  medname = "sglt2_Canagliflozin", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-5mo-14days",
  valsclass = "fac"
)
rsdata <- create_medvar(
  atc = "^(A10BK04|A10BD23|A10BD24)",
  medname = "sglt2_Ertugliflozin", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-5mo-14days",
  valsclass = "fac"
)
rsdata <- create_medvar(
  atc = "^A10BK05",
  medname = "sglt2_Ipragliflozin", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-5mo-14days",
  valsclass = "fac"
)
rsdata <- create_medvar(
  atc = "^A10BK06",
  medname = "sglt2_Sotagliflozin", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-5mo-14days",
  valsclass = "fac"
)

rm(lmtmp2)

# New/prevalent users -----------------------------------------------------

lmprevfunc <- function(timestart, timestop, medname) {
  lmtmp2 <- lmtmp %>%
    mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
    filter(diff <= timestart & diff >= timestop) %>%
    select(lopnr, shf_indexdtm, EDATUM, ATC)

  rsdata <<- create_medvar(
    atc = global_sglt2atc,
    medname = medname, cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = NA,
    valsclass = "fac"
  )
}

lmprevfunc(timestart = -2 * 365, timestop = -20 * 365, "sglt2prevuser1")
lmprevfunc(-30.5 * 5 + 1, -2 * 365 - 1, "sglt2prevuser2")
lmprevfunc(-1, -30.5 * 5, "sglt2prevuser3")
lmprevfunc(timestart = 14, timestop = 0, "sglt2prevuser4")

rsdata <- rsdata %>%
  mutate(
    sos_ddr_sglt2prevusers = factor(case_when(
      sos_ddr_sglt2prevuser1 == "Yes" ~ 5,
      sos_ddr_sglt2prevuser2 == "Yes" ~ 4,
      sos_ddr_sglt2prevuser3 == "Yes" ~ 3,
      sos_ddr_sglt2prevuser4 == "Yes" ~ 2,
      TRUE ~ 1
    ),
    levels = 1:5, labels = c(
      "No previous use",
      "Index-14 days",
      "5m-<Index",
      "5m-2 years",
      ">= 2 years"
    )
    ),
    sos_ddr_sglt2num = if_else(sos_ddr_sglt2 == "Yes", 1, 0),
  )


# Discontinuation ---------------------------------------------------------

lmlastprev <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= -30.5 * 5, diff <= 14) %>%
  group_by(lopnr) %>%
  arrange(EDATUM) %>%
  slice(n()) %>%
  select(lopnr, shf_indexdtm, EDATUM)

lmafter <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff > 14) %>%
  select(lopnr, shf_indexdtm, EDATUM)

# Patients with more prescriptions after index
lmdisc_1 <- bind_rows(
  lmlastprev,
  lmafter
) %>%
  group_by(lopnr) %>%
  arrange(EDATUM) %>%
  mutate(
    lagedatum = lag(EDATUM),
    diff = as.numeric(EDATUM - lagedatum)
  ) %>%
  ungroup() %>%
  arrange(lopnr, EDATUM) %>%
  mutate(
    disc = if_else(diff >= 5 * 30, 1, 0),
    disc_dtm = if_else(diff >= 5 * 30, lagedatum + 3 * 30, NA_Date_)
  ) %>%
  filter(disc == 1) %>%
  group_by(lopnr) %>%
  arrange(disc_dtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, disc, disc_dtm)

# Patients without more prescriptions after index
lmdisc_2 <- anti_join(lmlastprev,
  lmafter,
  by = c("lopnr", "shf_indexdtm")
) %>%
  mutate(
    disc = 1,
    disc_dtm = EDATUM + 3 * 30
  ) %>%
  select(lopnr, disc, disc_dtm)

lmdisc <- bind_rows(lmdisc_1, lmdisc_2)

rsdata <- left_join(rsdata, lmdisc, by = "lopnr")

rsdata <- rsdata %>%
  mutate(
    disc = case_when(
      sos_ddr_sglt2 == "No" ~ NA_real_,
      is.na(disc) ~ 0,
      censdtm < disc_dtm ~ 0,
      TRUE ~ disc
    ),
    disc_dtm2 = pmin(disc_dtm, censdtm, na.rm = T),
    time_disc = as.numeric(disc_dtm2 - (shf_indexdtm)),
    time_disc = if_else(time_disc < 0, 0, time_disc)
  ) %>%
  select(-disc_dtm, -disc_dtm2)

# Overtime graph ----------------------------------------------------------

overtimefunc <- function(year, halfyear, lmdata, rspop) {
  halfyeartmp <- ymd(paste0(year, "-", ifelse(halfyear == 1, "01", "07"), "-01"))

  popyear <- rspop %>%
    filter(
      shf_indexdtm <= halfyeartmp,
      censdtm >= halfyeartmp
    ) %>%
    select(lopnr, shf_indexdtm)

  lmyear <- inner_join(
    popyear,
    lmdata %>%
      filter(AR == year & halfyeare == halfyear),
    by = "lopnr"
  ) %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup()

  out <- c(
    year = year,
    halfyear = halfyear,
    den = popyear %>% count() %>% pull(n),
    num = lmyear %>% count() %>% pull(n)
  )
}

overtimefunc2 <- function(atc, rspop) {
  lmovertime <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc),
      halfyeare = semester(EDATUM)
    ) %>%
    filter(atcneed)

  overtime <- overtimefunc(year = 2021, halfyear = 1, lmdata = lmovertime, rspop)
  overtime <- rbind(overtime, overtimefunc(2021, 2, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2022, 1, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2022, 2, lmdata = lmovertime, rspop))

  overtime <- overtime %>%
    as.data.frame() %>%
    mutate(
      percent = as.numeric(num) / as.numeric(den) * 100,
      yearsemester = paste0(year, ":", halfyear),
      count = 1:n()
    )
}

overtimeall <- overtimefunc2(atc = global_sglt2atc, rspop = rsdata)
overtimedapa <- overtimefunc2(atc = "^(A10BK01|A10BD15|A10BD21|A10BD25)", rspop = rsdata)
overtimeempa <- overtimefunc2(atc = "^(A10BK03|A10BD19|A10BD20)", rspop = rsdata)

overtimenockd <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_gfrckdepi_cat) & shf_gfrckdepi_cat == ">=60")
)
overtimeckd <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_gfrckdepi_cat) & shf_gfrckdepi_cat == "<60")
)

overtimenotype2 <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_sos_com_diabetestype) & shf_sos_com_diabetestype == "No")
)
overtimetype2 <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_sos_com_diabetestype) & shf_sos_com_diabetestype == "Type II")
)

overtimenoprevhfh <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_sos_prevhfh) & shf_sos_prevhfh == "No previous HFH <1 year")
)
overtimeprevhfh <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_sos_prevhfh) & shf_sos_prevhfh == "Previous HFH <1 year")
)

overtimemale <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_sex == "Male")
)
overtimefemale <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_sex == "Female")
)

overtimeinpat <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_location == "In-patient")
)
overtimeoutpat <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_location == "Out-patient")
)

overtimedurhfless6mo <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_durationhf == "<6mo")
)
overtimedurhfmore6mo <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_durationhf == ">6mo")
)

overtime <- bind_rows(
  overtimeall %>% mutate(var = "all"),
  overtimedapa %>% mutate(var = "dapa"),
  overtimeempa %>% mutate(var = "empa"),
  overtimenockd %>% mutate(var = "nockd"),
  overtimeckd %>% mutate(var = "ckd"),
  overtimenotype2 %>% mutate(var = "notype2"),
  overtimetype2 %>% mutate(var = "type2"),
  overtimenoprevhfh %>% mutate(var = "noprevhfh"),
  overtimeprevhfh %>% mutate(var = "prevhfh"),
  overtimemale %>% mutate(var = "male"),
  overtimefemale %>% mutate(var = "female"),
  overtimeinpat %>% mutate(var = "inpat"),
  overtimeoutpat %>% mutate(var = "outpat"),
  overtimedurhfless6mo %>% mutate(var = "durhfless"),
  overtimedurhfmore6mo %>% mutate(var = "durhfmore")
)

# subset No type I diabetes, no eGFR < 20, no dialysis

overtimeall <- overtimefunc2(atc = global_sglt2atc, rspop = rsdata %>% filter(subset))
overtimedapa <- overtimefunc2(atc = "^(A10BK01|A10BD15|A10BD21|A10BD25)", rspop = rsdata %>% filter(subset))
overtimeempa <- overtimefunc2(atc = "^(A10BK03|A10BD19|A10BD20)", rspop = rsdata %>% filter(subset))

overtimenockd <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_gfrckdepi_cat) & shf_gfrckdepi_cat == ">=60" & subset)
)
overtimeckd <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_gfrckdepi_cat) & shf_gfrckdepi_cat == "<60" & subset)
)

overtimenotype2 <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_sos_com_diabetestype) & shf_sos_com_diabetestype == "No" & subset)
)
overtimetype2 <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_sos_com_diabetestype) & shf_sos_com_diabetestype == "Type II" & subset)
)

overtimenoprevhfh <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_sos_prevhfh) & shf_sos_prevhfh == "No previous HFH <1 year" & subset)
)
overtimeprevhfh <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_sos_prevhfh) & shf_sos_prevhfh == "Previous HFH <1 year" & subset)
)

overtimemale <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_sex == "Male" & subset)
)
overtimefemale <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_sex == "Female" & subset)
)

overtimeinpat <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_location == "In-patient" & subset)
)
overtimeoutpat <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_location == "Out-patient" & subset)
)

overtimedurhfless6mo <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_durationhf == "<6mo" & subset)
)
overtimedurhfmore6mo <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(shf_durationhf == ">6mo" & subset)
)

overtime_subset <- bind_rows(
  overtimeall %>% mutate(var = "all"),
  overtimedapa %>% mutate(var = "dapa"),
  overtimeempa %>% mutate(var = "empa"),
  overtimenockd %>% mutate(var = "nockd"),
  overtimeckd %>% mutate(var = "ckd"),
  overtimenotype2 %>% mutate(var = "notype2"),
  overtimetype2 %>% mutate(var = "type2"),
  overtimenoprevhfh %>% mutate(var = "noprevhfh"),
  overtimeprevhfh %>% mutate(var = "prevhfh"),
  overtimemale %>% mutate(var = "male"),
  overtimefemale %>% mutate(var = "female"),
  overtimeinpat %>% mutate(var = "inpat"),
  overtimeoutpat %>% mutate(var = "outpat"),
  overtimedurhfless6mo %>% mutate(var = "durhfless"),
  overtimedurhfmore6mo %>% mutate(var = "durhfmore")
)

metalm <- metalm[1:7, ]
metalm[, "Register"] <- "Prescribed Drug Register"
