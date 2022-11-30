
# Antidiabetic treatments from DDR ----------------------------------------

# within 5 months prior to index

lmtmp <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm),
  lmsel,
  by = "lopnr"
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


rm(lmtmp2)

# New/prevalent users -----------------------------------------------------

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff < -30.5 * 5) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rsdata <- create_medvar(
  atc = global_sglt2atc,
  medname = "sglt2prevusers", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-5mo-14days",
  valsclass = "fac"
)

# Overtime graph ----------------------------------------------------------


overtimefunc <- function(year, month, lmdata, rspop) {
  monthmid <- ymd(paste0(year, "-", month, "-15"))

  popyear <- rspop %>%
    filter(
      shf_indexdtm <= monthmid,
      censdtm >= monthmid
    )

  lmyear <- inner_join(popyear,
    lmdata %>%
      filter(AR == year & monthe == month),
    by = "lopnr"
  ) %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup()

  out <- c(year = year, month = month, den = popyear %>% count() %>% pull(n), num = lmyear %>% count() %>% pull(n))
}

overtimefunc2 <- function(atc, rspop) {
  lmovertime <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc),
      monthe = month(EDATUM)
    ) %>%
    filter(atcneed)

  overtime <- overtimefunc(2020, "11", lmdata = lmovertime, rspop)
  overtime <- rbind(overtime, overtimefunc(2020, 12, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(year = 2021, month = 1, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 2, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 3, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 4, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 5, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 6, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 7, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 8, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 9, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 10, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 11, lmdata = lmovertime, rspop))
  overtime <- rbind(overtime, overtimefunc(2021, 12, lmdata = lmovertime, rspop))

  overtime <- overtime %>%
    as.data.frame() %>%
    mutate(
      percent = as.numeric(num) / as.numeric(den) * 100,
      yearmonth = paste0(year, ":", ifelse(nchar(month) == 1, paste0("0", month), month)),
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
  rspop = rsdata %>% filter(!is.na(shf_diabetestype) & shf_diabetestype == "No")
)
overtimetype2 <- overtimefunc2(
  atc = global_sglt2atc,
  rspop = rsdata %>% filter(!is.na(shf_diabetestype) & shf_diabetestype == "Type II")
)


overtime <- bind_rows(
  overtimeall %>% mutate(var = "all"),
  overtimedapa %>% mutate(var = "dapa"),
  overtimeempa %>% mutate(var = "empa"),
  overtimenockd %>% mutate(var = "nockd"),
  overtimeckd %>% mutate(var = "ckd"),
  overtimenotype2 %>% mutate(var = "notype2"),
  overtimetype2 %>% mutate(var = "type2")
)
