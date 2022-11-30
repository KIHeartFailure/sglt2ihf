

# Additional variables from mainly SHF ------------------------------------

# ntprobnp

mednt <- median(rsdata$shf_ntprobnp, na.rm = T)

# income

medinc <- median(rsdata$scb_dispincome, na.rm = T)

rsdata <- rsdata %>%
  mutate(
    sos_ddr_sglt2num = if_else(sos_ddr_sglt2 == "Yes", 1, 0),
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
    shf_indexyearmonth = zoo::as.yearmon(shf_indexdtm),
    shf_indexyearmonth_num = as.numeric(factor(shf_indexyearmonth)),
    shf_indexyearmonth = factor(shf_indexyearmonth),
    shf_indexyearmonth_cat = factor(case_when(
      shf_indexdtm < ymd("2021-03-01") ~ 1,
      shf_indexdtm < ymd("2021-08-01") ~ 2,
      shf_indexdtm >= ymd("2021-08-01") ~ 3
    ),
    levels = 1:3, labels = c(
      "Nov 2020-Feb 2021",
      "March-July 2021",
      "Aug-Dec 2021"
    )
    ),

    # Outcomes

    # comp event outcomes
    sos_out_deathcvhosphf_comp = case_when(
      sos_out_deathcvhosphf == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
    sos_out_deathcv_comp = case_when(
      sos_out_deathcv == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
    sos_out_hosphf_comp = case_when(
      sos_out_hosphf == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
    sos_out_hosprenal_comp = case_when(
      sos_out_hosprenal == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    )
  ) %>%
  mutate(across(where(is.character), as.factor))
